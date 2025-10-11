//! Experimental animation controllers for Bevy.

use crate::math::{InterpolationPoint1d, InterpolationResult};

use arrayvec::ArrayVec;
use bevy::{
    animation::{
        AnimationClip, AnimationPlayer, AnimationTarget, AnimationTargetId, RepeatAnimation,
        graph::{AnimationGraphHandle, AnimationNodeIndex},
    },
    app::{AnimationSystems, App, Plugin, PostUpdate, PreUpdate, Update},
    asset::{AssetId, AssetServer, Assets, Handle},
    ecs::{
        component::{Component, Mutable},
        entity::Entity,
        hierarchy::Children,
        message::Message,
        name::Name,
        schedule::IntoScheduleConfigs as _,
        system::{Commands, Res, StaticSystemParam, SystemParam},
    },
    log::{debug, error, info, trace, warn},
    math::Vec2,
    platform::collections::HashMap,
    prelude::{AnimationGraph, Deref, DerefMut, Gltf, MessageWriter, Query, ResMut, With},
    reflect::Reflect,
    scene::SceneRoot,
    time::Time,
};
use by_address::ByAddress;
use smallvec::{SmallVec, smallvec};
use std::{any, cmp::Ordering, f32::consts::PI, iter, ops::Range, sync::Arc, time::Duration};

pub mod math;

pub struct AnimationControllersPlugin;

pub trait AnimatedCharacter: Component {
    type AnimationState;
    type SystemParam: SystemParam;

    const GROUP_COUNT: u32;

    fn compute_new_animation_state(
        &self,
        entity: Entity,
        param: &StaticSystemParam<'_, '_, Self::SystemParam>,
    ) -> Self::AnimationState;
    fn compute_animation_action(
        &self,
        group: AnimationGroup,
        new_state: &Self::AnimationState,
    ) -> AnimationAction;
    fn animation_for_state(
        group: AnimationGroup,
        state: &Self::AnimationState,
        param: &StaticSystemParam<'_, '_, Self::SystemParam>,
    ) -> (Option<AnimationBlend>, Duration);
    fn set_current_animation_state(&mut self, new_state: &Self::AnimationState);
}

/// How a character animation transition should occur.
#[derive(Clone, Copy, PartialEq, Reflect, Debug)]
pub enum AnimationAction {
    /// Keep the animation playing as is.
    NoChange,
    /// Change the time of the current 1D or 2D blend animation.
    ChangeTime,
    /// Change the animation, and restart it.
    ChangeAndRestart,
}

/// A fancier version of [`bevy::animation::AnimationTransitions`].
#[derive(Clone, Component, Reflect, Debug)]
pub struct AnimationController {
    pub groups: Vec<AnimationGroupController>,
}

#[derive(Clone, Default, Reflect, Debug)]
pub struct AnimationGroupController {
    pub main_animation: Option<PlayingAnimation>,
    pub transitions: Vec<AnimationTransition>,
}

#[derive(Clone, PartialEq, Reflect, Debug)]
pub enum PlayingAnimation {
    Single(AnimationNodeIndex),
    Blend1d {
        blend: RetargetedAnimationBlend1d,
        time: f32,
    },
    Blend2d {
        blend: RetargetedAnimationBlend2d,
        time: Vec2,
    },
}

#[derive(Clone, Reflect, Debug)]
pub struct AnimationTransition {
    pub current_weight: f32,
    pub weight_decline_per_sec: f32,
    pub animation: AnimationNodeIndex,
}

/// A mask value.
///
/// For example, this could be used to animate a character's upper body and
/// lower body separately.
#[derive(Clone, Copy, Reflect, PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Deref, DerefMut)]
pub struct AnimationGroup(pub u32);

pub enum AnimationBlend {
    Single(AssetId<AnimationClip>),
    Blend1d {
        blend: ByAddress<Arc<AnimationBlend1d>>,
        time: f32,
    },
    Blend2d {
        blend: ByAddress<Arc<AnimationBlend2d>>,
        time: Vec2,
    },
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum AnimationTag {
    Single(AssetId<AnimationClip>),
    Blend1d(ByAddress<Arc<AnimationBlend1d>>),
    Blend2d(ByAddress<Arc<AnimationBlend2d>>),
}

#[derive(Reflect, Deref, DerefMut, Debug)]
pub struct AnimationBlend1d(pub SmallVec<[AnimationBlendStop1d; INLINE_ANIMATION_BLENDS]>);

#[derive(Reflect, Debug)]
pub struct AnimationBlend2d {
    pub center: AssetId<AnimationClip>,
    pub rings: SmallVec<[AnimationBlendRing2d; INLINE_ANIMATION_RINGS]>,
}

#[derive(Reflect, Debug)]
pub struct AnimationBlendStop1d {
    pub clip: AssetId<AnimationClip>,
    pub time: f32,
}

#[derive(Reflect, Debug)]
pub struct AnimationBlendRing2d {
    pub stops: SmallVec<[AnimationBlendStop1d; INLINE_ANIMATION_BLENDS]>,
    pub time: f32,
}

// FIXME: The way this duplicates the above but with handles is extremely ugly…

pub struct AnimationTagHandle {
    pub tag: AnimationTag,
    pub handles: SmallVec<[Handle<AnimationClip>; INLINE_ANIMATION_BLENDS]>,
}

#[derive(Deref, DerefMut)]
pub struct AnimationBlendHandle1d(
    pub SmallVec<[AnimationBlendStopHandle1d; INLINE_ANIMATION_BLENDS]>,
);

pub struct AnimationBlendStopHandle1d {
    pub clip: Handle<AnimationClip>,
    pub time: f32,
}

/// Fired when all animations for an entity have been retargeted.
#[derive(Message, Reflect, Deref, DerefMut)]
pub struct AnimationsRetargetedEvent(pub Entity);

pub struct AnimationRetargetGroup {
    pub animations: Vec<(AnimationTagHandle, RepeatAnimation)>,
    pub graph_node: AnimationNodeIndex,
}

#[derive(Clone, Component, Default, Deref, DerefMut)]
pub struct RetargetedAnimations(HashMap<(AnimationGroup, AnimationTag), RetargetedAnimation>);

#[derive(Clone, Reflect)]
pub struct RetargetedAnimation {
    pub nodes: RetargetedAnimationNodes,
    pub repeat: RepeatAnimation,
}

#[derive(Clone, Reflect, Debug)]
pub enum RetargetedAnimationNodes {
    Single(AnimationNodeIndex),
    Blend1d(RetargetedAnimationBlend1d),
    Blend2d(RetargetedAnimationBlend2d),
}

#[derive(Clone, PartialEq, Reflect, Debug, Deref, DerefMut)]
pub struct RetargetedAnimationBlend1d(
    pub SmallVec<[RetargetedAnimationBlendStop1d; INLINE_ANIMATION_BLENDS]>,
);

#[derive(Clone, PartialEq, Reflect, Debug)]
pub struct RetargetedAnimationBlendStop1d {
    pub node: AnimationNodeIndex,
    pub time: f32,
}

#[derive(Clone, PartialEq, Reflect, Debug)]
pub struct RetargetedAnimationBlend2d {
    pub stops: SmallVec<[RetargetedAnimationBlendStop1d; INLINE_ANIMATION_BLENDS]>,
    pub rings: SmallVec<[RetargetedAnimationBlendRing2d; INLINE_ANIMATION_RINGS]>,
}

#[derive(Clone, PartialEq, Reflect, Debug)]
pub struct RetargetedAnimationBlendRing2d {
    pub first_stop_index: usize,
    pub time: f32,
}

#[derive(Component, Reflect)]
pub struct AnimationRetargeter {
    #[reflect(ignore)]
    pub groups: Vec<AnimationRetargetGroup>,
    pub dest_root_joint: Name,
    /// Whether the animation path should start with the destination root joint.
    pub include_dest_root_joint_in_path: bool,
}

#[derive(Reflect)]
pub struct AnimationRetarget {
    pub gltf: Handle<Gltf>,
}

pub struct PlayingAnimationIterator<'a> {
    animation: &'a PlayingAnimation,
    index: usize,
}

#[derive(Debug)]
struct LinearInterpolationWeights {
    prev_stop_index: usize,
    next_stop_index: usize,
    time: f32,
}

/// The number of animation blends we store inline in a [`SmallVec`].
const INLINE_ANIMATION_BLENDS: usize = 5;

const INLINE_ANIMATION_RINGS: usize = 2;

impl AnimationController {
    pub fn new(group_count: u32) -> AnimationController {
        AnimationController {
            groups: (0..group_count)
                .map(|_| AnimationGroupController::default())
                .collect(),
        }
    }

    pub fn group_mut(&mut self, index: AnimationGroup) -> &mut AnimationGroupController {
        &mut self.groups[index.0 as usize]
    }
}

impl AnimationGroupController {
    /// Plays a new animation on the given [`AnimationPlayer`], fading out any
    /// existing animations that were already playing over the
    /// `transition_duration`.
    ///
    /// Pass [`Duration::ZERO`] to instantly switch to a new animation, avoiding
    /// any transition.
    pub fn play(
        &mut self,
        player: &mut AnimationPlayer,
        new_animation: PlayingAnimation,
        transition_duration: Duration,
        repeat: RepeatAnimation,
        action: AnimationAction,
    ) {
        match action {
            AnimationAction::ChangeAndRestart => {
                self.transition_away_from_old_animation(
                    player,
                    new_animation.clone(),
                    transition_duration,
                );

                // If already transitioning away from this animation, cancel the transition.
                // Otherwise the transition ending would incorrectly stop the new animation.
                // FIXME: O(n^2), is this bad?
                self.transitions.retain(|transition| {
                    new_animation.iter().all(|new_playing_animation_node| {
                        *new_playing_animation_node != transition.animation
                    })
                });

                self.start_playing_new_animation(player, new_animation, repeat);
            }

            AnimationAction::ChangeTime => {
                self.change_animation_time(player, new_animation, repeat)
            }

            AnimationAction::NoChange => {}
        }
    }

    fn transition_away_from_old_animation(
        &mut self,
        player: &mut AnimationPlayer,
        new_animation: PlayingAnimation,
        transition_duration: Duration,
    ) {
        let Some(old_playing_animation) = self.main_animation.replace(new_animation) else {
            return;
        };

        for old_playing_animation_node in old_playing_animation.iter() {
            if transition_duration.is_zero() {
                // Immediately stop the old animation.
                trace!("Immediately stopping {:?}", *old_playing_animation_node);
                player.stop(*old_playing_animation_node);
                continue;
            }

            // Enqueue a transition.
            // FIXME: Is this right?
            let Some(old_animation) = player.animation_mut(*old_playing_animation_node) else {
                info!(
                    "Couldn't find active animation for {:?}",
                    old_playing_animation_node
                );
                continue;
            };
            if old_animation.is_paused() {
                info!(
                    "Animation {:?} is paused, not enqueuing transition",
                    old_playing_animation_node
                );
                continue;
            }
            trace!(
                "Enqueuing transition from {:?}",
                *old_playing_animation_node
            );
            self.transitions.push(AnimationTransition {
                current_weight: old_animation.weight(),
                weight_decline_per_sec: 1.0 / transition_duration.as_secs_f32(),
                animation: *old_playing_animation_node,
            });
        }
    }

    fn start_playing_new_animation(
        &mut self,
        player: &mut AnimationPlayer,
        new_animation: PlayingAnimation,
        repeat: RepeatAnimation,
    ) {
        match new_animation {
            PlayingAnimation::Single(node_index) => {
                player.start(node_index).set_repeat(repeat);
            }

            PlayingAnimation::Blend1d { blend, time } => {
                let InterpolationResult {
                    prev_index,
                    next_index,
                    weight,
                } = math::linearly_interpolate_data(&blend, time);

                trace!(
                    "Interpolating between stop {:?} and {:?}: time {:?} prev time {:?} \
                     next time {:?} next weight {:?}",
                    prev_index,
                    next_index,
                    time,
                    blend[prev_index].time,
                    next_index.map(|next_index| blend[next_index].time),
                    weight
                );

                player
                    .start(blend[prev_index].node)
                    .set_repeat(repeat)
                    .set_weight(1.0 - weight);

                if let Some(next_index) = next_index {
                    player
                        .start(blend[next_index].node)
                        .set_repeat(repeat)
                        .set_weight(weight);
                }
            }

            PlayingAnimation::Blend2d { blend, time } => {
                let weights = polar_bilinear_interpolate(&blend, time);

                for (stop, weight) in blend.stops.iter().zip(weights.iter()) {
                    player
                        .start(stop.node)
                        .set_repeat(repeat)
                        .set_weight(*weight);
                }
            }
        }
    }

    fn change_animation_time(
        &mut self,
        player: &mut AnimationPlayer,
        new_animation: PlayingAnimation,
        repeat: RepeatAnimation,
    ) {
        let Some(ref mut main_animation) = self.main_animation else {
            warn!("Attempted to change animation time, but no main animation was playing");
            return;
        };

        match (new_animation, main_animation) {
            (PlayingAnimation::Single(node_index), _) => {
                error!(
                    "Attempted to change the time of a non-blend: {:?}",
                    node_index
                );
            }

            (
                PlayingAnimation::Blend1d { blend, time },
                &mut PlayingAnimation::Blend1d {
                    time: ref mut playing_time,
                    blend: ref playing_blend,
                },
            ) => {
                // FIXME: This should compare by pointer!
                if blend != *playing_blend {
                    warn!("Attempted to change the time of a 1D blend that wasn't playing");
                    return;
                }

                *playing_time = time;

                let InterpolationResult {
                    prev_index,
                    next_index,
                    weight,
                } = math::linearly_interpolate_data(&blend, time);

                trace!(
                    "Interpolating between stop {:?} and {:?}: time {:?} prev time {:?} \
                     next time {:?} next weight {:?}",
                    prev_index,
                    next_index,
                    time,
                    blend[prev_index].time,
                    next_index.map(|next_index| blend[next_index].time),
                    weight
                );

                player
                    .play(blend[prev_index].node)
                    .set_repeat(repeat)
                    .set_weight(1.0 - weight);

                if let Some(next_index) = next_index {
                    player
                        .play(blend[next_index].node)
                        .set_repeat(repeat)
                        .set_weight(weight);
                }
            }

            (PlayingAnimation::Blend1d { .. }, _) => {
                warn!("Attempted to change the time of a 1D blend, but a 1D blend wasn't playing");
            }

            (
                PlayingAnimation::Blend2d { blend, time },
                &mut PlayingAnimation::Blend2d {
                    time: ref mut playing_time,
                    blend: ref playing_blend,
                },
            ) => {
                // FIXME: This should compare by pointer!
                if blend != *playing_blend {
                    warn!("Attempted to change the time of a 2D blend that wasn't playing");
                    return;
                }

                *playing_time = time;

                let weights = polar_bilinear_interpolate(&blend, time);

                for (stop, weight) in blend.stops.iter().zip(weights.iter()) {
                    player
                        .play(stop.node)
                        .set_repeat(repeat)
                        .set_weight(*weight);
                }
            }

            (PlayingAnimation::Blend2d { .. }, _) => {
                warn!("Attempted to change the time of a 2D blend, but a 2D blend wasn't playing");
            }
        }
    }
}

pub fn animate_character<A: AnimatedCharacter + Component<Mutability = Mutable>>(
    mut q_characters: Query<(Entity, &mut A, &Children)>,
    mut q_rigs: Query<(
        &mut AnimationController,
        &mut AnimationPlayer,
        &RetargetedAnimations,
    )>,
    param: StaticSystemParam<A::SystemParam>,
) {
    for (entity, mut animated_character, children) in &mut q_characters {
        let Some(rig) = children.iter().cloned().find(|&kid| q_rigs.contains(kid)) else {
            error!(
                "Couldn't find character meshes for {:?}",
                any::type_name::<A>()
            );
            continue;
        };
        let Ok((mut animation_controller, mut animation_player, retargeted_animations)) =
            q_rigs.get_mut(rig)
        else {
            error!(
                "Couldn't find a necessary animation component for {:?}",
                any::type_name::<A>()
            );
            continue;
        };

        let new_animation_state = animated_character.compute_new_animation_state(entity, &param);
        let mut any_dirty = false;
        for group in 0..A::GROUP_COUNT {
            let group = AnimationGroup(group);
            let animation_action =
                animated_character.compute_animation_action(group, &new_animation_state);
            if animation_action == AnimationAction::NoChange {
                continue;
            }
            any_dirty = true;

            let (Some(new_animation_blend), animation_transition_time) =
                A::animation_for_state(group, &new_animation_state, &param)
            else {
                continue;
            };
            let Some(RetargetedAnimation { nodes, repeat }) =
                retargeted_animations.get(&(group, new_animation_blend.tag()))
            else {
                warn!(
                    "Couldn't find animation for {:?}, group {:?}, tag {:?}",
                    any::type_name::<A>(),
                    group,
                    new_animation_blend.tag()
                );
                continue;
            };

            debug!(
                "Changing animation for {:?}, group {:?}",
                any::type_name::<A>(),
                group
            );

            let playing_animation = match (new_animation_blend, nodes) {
                (AnimationBlend::Single(_), RetargetedAnimationNodes::Single(node_index)) => {
                    PlayingAnimation::Single(*node_index)
                }
                (
                    AnimationBlend::Blend1d { blend: _, time },
                    RetargetedAnimationNodes::Blend1d(blend),
                ) => PlayingAnimation::Blend1d {
                    blend: (*blend).clone(),
                    time,
                },
                (
                    AnimationBlend::Blend2d { blend: _, time },
                    RetargetedAnimationNodes::Blend2d(blend),
                ) => PlayingAnimation::Blend2d {
                    blend: (*blend).clone(),
                    time,
                },
                _ => {
                    error!(
                        "Mismatch between blend and retargeted animation nodes for {:?}, \
                        group {:?}",
                        any::type_name::<A>(),
                        group
                    );
                    continue;
                }
            };

            animation_controller.group_mut(group).play(
                &mut animation_player,
                playing_animation,
                animation_transition_time,
                *repeat,
                animation_action,
            )
        }

        if any_dirty {
            animated_character.set_current_animation_state(&new_animation_state);
        }
    }
}

impl AnimationBlend {
    fn tag(&self) -> AnimationTag {
        match *self {
            AnimationBlend::Single(clip) => AnimationTag::Single(clip),
            AnimationBlend::Blend1d { ref blend, .. } => AnimationTag::Blend1d((*blend).clone()),
            AnimationBlend::Blend2d { ref blend, .. } => AnimationTag::Blend2d((*blend).clone()),
        }
    }
}

impl From<AssetId<AnimationClip>> for AnimationTag {
    fn from(clip: AssetId<AnimationClip>) -> Self {
        AnimationTag::Single(clip)
    }
}

impl From<AssetId<AnimationClip>> for AnimationBlend {
    fn from(clip: AssetId<AnimationClip>) -> Self {
        AnimationBlend::Single(clip)
    }
}

impl PlayingAnimation {
    fn iter(&self) -> PlayingAnimationIterator {
        PlayingAnimationIterator {
            animation: self,
            index: 0,
        }
    }
}

impl<'a> Iterator for PlayingAnimationIterator<'a> {
    type Item = &'a AnimationNodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.animation, self.index) {
            (PlayingAnimation::Single(node_index), 0) => {
                self.index += 1;
                Some(node_index)
            }
            (PlayingAnimation::Single(_), _) => None,
            (PlayingAnimation::Blend1d { blend, time: _ }, index) if index < blend.len() => {
                let stop = &blend[self.index];
                self.index += 1;
                Some(&stop.node)
            }
            (PlayingAnimation::Blend1d { .. }, _) => None,
            (PlayingAnimation::Blend2d { blend, time: _ }, index) if index < blend.stops.len() => {
                let stop = &blend.stops[self.index];
                self.index += 1;
                Some(&stop.node)
            }
            (PlayingAnimation::Blend2d { .. }, _) => None,
        }
    }
}

impl PlayingAnimation {
    /// This is only useful when you know the animation isn't a blend, because
    /// it doesn't supply a blend weight.
    pub fn from_retargeted_animation(
        retargeted_animation: &RetargetedAnimationNodes,
    ) -> PlayingAnimation {
        match retargeted_animation {
            RetargetedAnimationNodes::Single(node_index) => PlayingAnimation::Single(*node_index),

            RetargetedAnimationNodes::Blend1d(blend) => {
                warn!(
                    "`PlayingAnimation::from_retargeted_animation` called on a 1D blendtree; \
                     assuming time is zero"
                );
                PlayingAnimation::Blend1d {
                    blend: (*blend).clone(),
                    time: 0.0,
                }
            }

            RetargetedAnimationNodes::Blend2d(blend) => {
                warn!(
                    "`PlayingAnimation::from_retargeted_animation` called on a 2D blendtree; \
                    assuming time is (0.0, 0.0)"
                );
                PlayingAnimation::Blend2d {
                    blend: (*blend).clone(),
                    time: Vec2::ZERO,
                }
            }
        }
    }
}

impl AnimationBlendStopHandle1d {
    pub fn new(clip: Handle<AnimationClip>, time: f32) -> Self {
        Self { clip, time }
    }
}

impl AnimationBlendStop1d {
    pub fn new(clip: AssetId<AnimationClip>, time: f32) -> Self {
        Self { clip, time }
    }
}

impl AnimationTagHandle {
    pub fn new(
        tag: AnimationTag,
        handles: SmallVec<[Handle<AnimationClip>; INLINE_ANIMATION_BLENDS]>,
    ) -> Self {
        Self { tag, handles }
    }
}

impl From<Handle<AnimationClip>> for AnimationTagHandle {
    // Convenience function for making an `AnimationTagHandle::Single` from a
    // `Handle<AnimationClip>`.
    fn from(clip: Handle<AnimationClip>) -> Self {
        AnimationTagHandle {
            tag: AnimationTag::Single(clip.id()),
            handles: smallvec![clip],
        }
    }
}

impl InterpolationPoint1d for RetargetedAnimationBlendStop1d {
    fn time(&self) -> f32 {
        self.time
    }
}

pub fn polar_bilinear_interpolate(
    blend: &RetargetedAnimationBlend2d,
    time: Vec2,
) -> SmallVec<[f32; INLINE_ANIMATION_BLENDS]> {
    let next_ring_index = match blend
        .rings
        .binary_search_by(|ring| ring.time.partial_cmp(&time.x).unwrap_or(Ordering::Less))
    {
        Ok(next_ring_index) => next_ring_index,
        Err(next_ring_index) => next_ring_index,
    };

    let next_weights = if next_ring_index == blend.rings.len() - 1 {
        Some(linear_interpolate_ring(
            &blend.stops,
            (blend.rings[next_ring_index].first_stop_index)..(blend.stops.len()),
            time.y,
        ))
    } else if next_ring_index < blend.rings.len() - 1 {
        Some(linear_interpolate_ring(
            &blend.stops,
            (blend.rings[next_ring_index].first_stop_index)
                ..(blend.rings[next_ring_index + 1].first_stop_index),
            time.y,
        ))
    } else {
        None
    };

    let prev_weights = if next_ring_index > 0 && next_ring_index == blend.rings.len() {
        Some(linear_interpolate_ring(
            &blend.stops,
            (blend.rings[next_ring_index - 1].first_stop_index)..(blend.stops.len()),
            time.y,
        ))
    } else if next_ring_index > 0 {
        Some(linear_interpolate_ring(
            &blend.stops,
            (blend.rings[next_ring_index - 1].first_stop_index)
                ..(blend.rings[next_ring_index].first_stop_index),
            time.y,
        ))
    } else {
        None
    };

    let mut weights = smallvec![0.0; blend.stops.len()];

    match (prev_weights, next_weights) {
        (None, None) => weights[0] = 1.0,
        (Some(prev_weights), None) => {
            weights[prev_weights.prev_stop_index] = 1.0 - prev_weights.time;
            weights[prev_weights.next_stop_index] = prev_weights.time;
        }
        (None, Some(next_weights)) => {
            let radius_time = time.x / blend.rings[0].time;
            weights[0] = radius_time;
            weights[next_weights.prev_stop_index] = (1.0 - radius_time) * (1.0 - next_weights.time);
            weights[next_weights.next_stop_index] = (1.0 - radius_time) * next_weights.time;
        }
        (Some(prev_weights), Some(next_weights)) => {
            let prev_ring_index = next_ring_index - 1;
            let radius_time = (time.x - blend.rings[prev_ring_index].time)
                / (blend.rings[next_ring_index].time - blend.rings[prev_ring_index].time);
            weights[prev_weights.prev_stop_index] = radius_time * (1.0 - prev_weights.time);
            weights[prev_weights.next_stop_index] = radius_time * prev_weights.time;
            weights[next_weights.prev_stop_index] = (1.0 - radius_time) * (1.0 - next_weights.time);
            weights[next_weights.next_stop_index] = (1.0 - radius_time) * next_weights.time;
        }
    }

    weights
}

fn linear_interpolate_ring(
    stops: &[RetargetedAnimationBlendStop1d],
    stop_range: Range<usize>,
    time: f32,
) -> LinearInterpolationWeights {
    debug_assert!(!stops.is_empty());

    if stop_range.len() == 1 {
        return LinearInterpolationWeights {
            prev_stop_index: stop_range.start,
            next_stop_index: stop_range.end,
            time: 0.0,
        };
    }

    let time = time.rem_euclid(2.0 * PI);

    let mut next_stop_index = stop_range.start
        + match stops[stop_range.clone()]
            .binary_search_by(|stop| stop.time.partial_cmp(&time).unwrap_or(Ordering::Less))
        {
            Ok(next_stop_index) => next_stop_index,
            Err(next_stop_index) => next_stop_index,
        };
    if next_stop_index == stop_range.end {
        next_stop_index = stop_range.start;
    }
    let prev_stop_index = if next_stop_index == stop_range.start {
        stop_range.end - 1
    } else {
        next_stop_index - 1
    };

    let prev_time = stops[prev_stop_index].time;
    let mut next_time = stops[next_stop_index].time;
    if next_time < prev_time {
        next_time += 2.0 * PI;
    }

    let time = (time - prev_time) / (next_time - prev_time);

    LinearInterpolationWeights {
        prev_stop_index,
        next_stop_index,
        time,
    }
}

impl Plugin for AnimationControllersPlugin {
    fn build(&self, app: &mut App) {
        app.add_message::<AnimationsRetargetedEvent>()
            .add_systems(PreUpdate, prepare_retargeting)
            .add_systems(Update, retarget_animations)
            .add_systems(
                PostUpdate,
                (advance_transitions, expire_completed_transitions)
                    .before(bevy::animation::animate_targets)
                    .in_set(AnimationSystems),
            );
    }
}

/// A system that alters the weight of currently-playing transitions based on
/// the current time and decline amount.
pub fn advance_transitions(
    mut q_animations: Query<(&mut AnimationController, &mut AnimationPlayer)>,
    time: Res<Time>,
) {
    // We use a "greedy layer" system here. The top layer (most recent
    // transition) gets as much as weight as it wants, and the remaining amount
    // is divided between all the other layers, eventually culminating in the
    // currently-playing animation receiving whatever's left. This results in a
    // nicely normalized weight.
    let mut remaining_weight = 1.0;
    for (mut animation_controller, mut player) in q_animations.iter_mut() {
        for animation_group_controller in animation_controller.groups.iter_mut() {
            for transition in &mut animation_group_controller.transitions.iter_mut().rev() {
                // Decrease weight.
                transition.current_weight = (transition.current_weight
                    - transition.weight_decline_per_sec * time.delta_secs())
                .max(0.0);

                // Update weight.
                let Some(ref mut animation) = player.animation_mut(transition.animation) else {
                    warn!(
                        "Failed to find active animation for {:?}",
                        transition.animation
                    );
                    continue;
                };
                animation.set_weight(transition.current_weight * remaining_weight);
                remaining_weight -= animation.weight();
            }

            // Distribute remaining weight.
            //
            // FIXME: This evaluates animations twice! Such a botch!
            if let Some(main_animation) = &animation_group_controller.main_animation {
                distribute_weight(&mut player, main_animation, remaining_weight);
            }
        }
    }
}

fn distribute_weight(
    player: &mut AnimationPlayer,
    main_animation: &PlayingAnimation,
    remaining_weight: f32,
) {
    match main_animation {
        PlayingAnimation::Single(node_index) => {
            if let Some(ref mut animation) = player.animation_mut(*node_index) {
                animation.set_weight(remaining_weight);
            }
        }

        PlayingAnimation::Blend1d { blend, time } => {
            // Evaluate blend and distribute remaining weight.

            let InterpolationResult {
                prev_index,
                next_index,
                weight: interpolated_weight,
            } = math::linearly_interpolate_data(blend, *time);

            trace!(
                "Distributing weight: Interpolating between stop {:?} and {:?}: time {:?} prev \
                    time {:?} next time {:?} next weight {:?}",
                prev_index,
                next_index,
                time,
                blend[prev_index].time,
                next_index.map(|next_index| blend[next_index].time),
                interpolated_weight
            );

            for (blend_stop_index, blend_stop) in blend.0.iter().enumerate() {
                let Some(ref mut animation) = player.animation_mut(blend_stop.node) else {
                    continue;
                };
                let weight = if blend_stop_index == prev_index {
                    remaining_weight * (1.0 - interpolated_weight)
                } else if Some(blend_stop_index) == next_index {
                    remaining_weight * interpolated_weight
                } else {
                    0.0
                };
                animation.set_weight(weight);
            }
        }

        PlayingAnimation::Blend2d { blend, time } => {
            let weights = polar_bilinear_interpolate(blend, *time);

            for (stop, interpolated_weight) in blend.stops.iter().zip(weights.iter()) {
                if let Some(ref mut animation) = player.animation_mut(stop.node) {
                    animation.set_weight(remaining_weight * *interpolated_weight);
                }
            }
        }
    }
}

/// A system that removed transitions that have completed from the
/// [`AnimationTransitions`] object.
pub fn expire_completed_transitions(
    mut q_animations: Query<(&mut AnimationController, &mut AnimationPlayer)>,
) {
    for (mut animation_controller, mut player) in q_animations.iter_mut() {
        for animation_group_controller in animation_controller.groups.iter_mut() {
            animation_group_controller.transitions.retain(|transition| {
                let expire = transition.current_weight <= 0.0;
                if expire {
                    player.stop(transition.animation);
                }
                !expire
            });
        }
    }
}

pub fn prepare_retargeting(
    mut commands: Commands,
    q_retargeting_targets: Query<(Entity, &AnimationRetargeter, &SceneRoot)>,
    q_candidate_targets: Query<(Option<&Name>, Option<&Children>)>,
    asset_server: Res<AssetServer>,
) {
    for (entity, animation_retargeter, target_scene) in &q_retargeting_targets {
        if !asset_server.is_loaded_with_dependencies(target_scene.id()) {
            continue;
        }

        commands.entity(entity).insert(AnimationPlayer::default());

        let debug_name = if cfg!(debug_assertions) {
            "".to_owned()
        } else {
            match asset_server.get_path(target_scene.id()) {
                Some(asset_path) => format!("{asset_path}"),
                None => format!("{:?}", target_scene.id()),
            }
        };

        search_for_root(
            &mut commands,
            &q_candidate_targets,
            animation_retargeter,
            entity,
            entity,
            &debug_name,
        );
    }

    fn search_for_root(
        commands: &mut Commands,
        q_candidate_targets: &Query<(Option<&Name>, Option<&Children>)>,
        animation_retargeter: &AnimationRetargeter,
        root_entity: Entity,
        entity: Entity,
        debug_name: &str,
    ) {
        let Ok((maybe_name, maybe_kids)) = q_candidate_targets.get(entity) else {
            return;
        };

        let mut path: ArrayVec<_, 1> = ArrayVec::new();
        if animation_retargeter.include_dest_root_joint_in_path {
            path.push(animation_retargeter.dest_root_joint.clone());
        }

        if maybe_name.is_some_and(|name| *name == animation_retargeter.dest_root_joint) {
            let Some(kids) = maybe_kids else { return };
            for &kid in kids {
                add_animation_targets(
                    commands,
                    q_candidate_targets,
                    root_entity,
                    kid,
                    &path,
                    debug_name,
                );
            }
            return;
        }

        let Some(kids) = maybe_kids else { return };
        for &kid in kids {
            search_for_root(
                commands,
                q_candidate_targets,
                animation_retargeter,
                root_entity,
                kid,
                debug_name,
            );
        }
    }

    fn add_animation_targets(
        commands: &mut Commands,
        q_candidate_targets: &Query<(Option<&Name>, Option<&Children>)>,
        root: Entity,
        entity: Entity,
        parent_path: &[Name],
        debug_name: &str,
    ) {
        let Ok((Some(name), maybe_kids)) = q_candidate_targets.get(entity) else {
            return;
        };

        let path: Vec<Name> = parent_path
            .iter()
            .cloned()
            .chain(iter::once(name.clone()))
            .collect();

        debug!("Adding animation target for {}: {:?}", debug_name, path);

        commands.entity(entity).insert(AnimationTarget {
            id: AnimationTargetId::from_names(path.iter()),
            player: root,
        });

        let Some(kids) = maybe_kids else { return };
        for &kid in kids {
            add_animation_targets(commands, q_candidate_targets, root, kid, &path, debug_name);
        }
    }
}

#[allow(clippy::too_many_arguments, clippy::type_complexity)]
pub fn retarget_animations(
    mut commands: Commands,
    mut q_retargeting_targets: Query<
        (
            Entity,
            &mut AnimationRetargeter,
            &mut RetargetedAnimations,
            &AnimationGraphHandle,
        ),
        With<AnimationPlayer>,
    >,
    mut animation_graph_assets: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
    mut animations_retargeted_events: MessageWriter<AnimationsRetargetedEvent>,
) {
    for (entity, mut animation_retargeter, mut retargeted_animations, animation_graph) in
        &mut q_retargeting_targets
    {
        for (group_index, group) in animation_retargeter.groups.iter_mut().enumerate() {
            let group_index = AnimationGroup(group_index as u32);
            group.animations.retain(|(clip, repeat)| {
                let animation_graph = animation_graph_assets
                    .get_mut(animation_graph.id())
                    .expect("Animation graph wasn't loaded");

                match &clip.tag {
                    AnimationTag::Single(_) => {
                        let clip_handle = clip.handles[0].clone();
                        if !asset_server.is_loaded_with_dependencies(clip_handle.id()) {
                            return true;
                        }

                        let clip_node_index =
                            animation_graph.add_clip(clip_handle.clone(), 1.0, group.graph_node);

                        retargeted_animations.insert(
                            (group_index, clip_handle.id().into()),
                            RetargetedAnimation {
                                nodes: RetargetedAnimationNodes::Single(clip_node_index),
                                repeat: *repeat,
                            },
                        );
                    }

                    AnimationTag::Blend1d(blend) => {
                        if !blend
                            .iter()
                            .all(|stop| asset_server.is_loaded_with_dependencies(stop.clip))
                        {
                            return true;
                        }

                        let blend_node_index = animation_graph.add_blend(1.0, group.graph_node);

                        let mut retargeted_stops = SmallVec::new();
                        for (input_stop, input_stop_clip) in blend.iter().zip(clip.handles.iter()) {
                            let node = animation_graph.add_clip(
                                input_stop_clip.clone(),
                                1.0,
                                blend_node_index,
                            );
                            retargeted_stops.push(RetargetedAnimationBlendStop1d {
                                node,
                                time: input_stop.time,
                            });
                        }

                        retargeted_animations.insert(
                            (group_index, AnimationTag::Blend1d(blend.clone())),
                            RetargetedAnimation {
                                nodes: RetargetedAnimationNodes::Blend1d(
                                    RetargetedAnimationBlend1d(retargeted_stops),
                                ),
                                repeat: *repeat,
                            },
                        );
                    }

                    AnimationTag::Blend2d(blend) => {
                        if !iter::once(blend.center)
                            .chain(
                                blend
                                    .rings
                                    .iter()
                                    .flat_map(|ring| ring.stops.iter())
                                    .map(|stop| stop.clip),
                            )
                            .all(|clip| asset_server.is_loaded_with_dependencies(clip))
                        {
                            return true;
                        }

                        match create_retargeted_animation_for_2d_blend(
                            animation_graph,
                            group.graph_node,
                            blend,
                            clip.handles.iter(),
                            repeat,
                        ) {
                            None => {
                                error!(
                                    "Mismatch between the number of clip handles for 2D blend \
                                     with center clip {:?}",
                                    blend.center
                                );
                            }
                            Some(retargeted_animation) => {
                                retargeted_animations.insert(
                                    (group_index, AnimationTag::Blend2d(blend.clone())),
                                    retargeted_animation,
                                );
                            }
                        }
                    }
                }

                false
            });
        }

        if animation_retargeter
            .groups
            .iter()
            .all(|group| group.animations.is_empty())
        {
            commands.entity(entity).remove::<AnimationRetargeter>();
            animations_retargeted_events.write(AnimationsRetargetedEvent(entity));
        }
    }
}

fn create_retargeted_animation_for_2d_blend<'a>(
    animation_graph: &mut AnimationGraph,
    animation_graph_blend_node: AnimationNodeIndex,
    blend: &AnimationBlend2d,
    mut clip_handles: impl Iterator<Item = &'a Handle<AnimationClip>>,
    repeat: &RepeatAnimation,
) -> Option<RetargetedAnimation> {
    let blend_node_index = animation_graph.add_blend(1.0, animation_graph_blend_node);

    let mut retargeted_rings = SmallVec::new();
    let mut retargeted_stops = smallvec![RetargetedAnimationBlendStop1d {
        node: animation_graph.add_clip(clip_handles.next()?.clone(), 1.0, blend_node_index),
        time: 0.0
    }];
    for input_ring in blend.rings.iter() {
        let first_stop_index = retargeted_stops.len();
        for input_stop in input_ring.stops.iter() {
            let node =
                animation_graph.add_clip(clip_handles.next()?.clone(), 1.0, blend_node_index);
            retargeted_stops.push(RetargetedAnimationBlendStop1d {
                node,
                time: input_stop.time,
            })
        }
        retargeted_rings.push(RetargetedAnimationBlendRing2d {
            first_stop_index,
            time: input_ring.time,
        });
    }

    Some(RetargetedAnimation {
        nodes: RetargetedAnimationNodes::Blend2d(RetargetedAnimationBlend2d {
            stops: retargeted_stops,
            rings: retargeted_rings,
        }),
        repeat: *repeat,
    })
}
