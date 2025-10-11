//! Experimental animation controllers for Bevy.

use crate::{
    math::InterpolationResult,
    retargeting::{
        AnimationAssetId, AnimationsRetargetedEvent, RetargetedAnimationBlend1d,
        RetargetedAnimationBlend2d, RetargetedAnimationBlendStop1d, RetargetedAnimationNodes,
    },
};

use bevy::{
    animation::{AnimationClip, AnimationPlayer, RepeatAnimation, graph::AnimationNodeIndex},
    app::{AnimationSystems, App, Plugin, PostUpdate, PreUpdate, Update},
    asset::{Asset, AssetApp, AssetId, Handle},
    ecs::{component::Component, schedule::IntoScheduleConfigs as _, system::Res},
    log::{error, info, trace, warn},
    math::Vec2,
    prelude::{Deref, DerefMut, Query, ReflectDeserialize, ReflectSerialize},
    reflect::Reflect,
    time::Time,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};
use smallvec::{SmallVec, smallvec};
use std::{cmp::Ordering, f32::consts::PI, ops::Range, time::Duration};

pub mod characters;
pub mod math;
pub mod retargeting;

pub struct AnimationControllersPlugin;

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

#[derive(Clone, Asset, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
pub struct AnimationBlendAsset {
    #[serde(flatten)]
    pub blend_type: AnimationBlendAssetType,
}

#[derive(Clone, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum AnimationBlendAssetType {
    Blend1d {
        stops: Vec<AnimationBlendAssetStop1d>,
    },
    Blend2d {
        center: AnimationBlendAssetClipHandle,
        rings: Vec<AnimationBlendAssetRing2d>,
    },
}

#[derive(Clone, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
pub struct AnimationBlendAssetStop1d {
    pub clip: AnimationBlendAssetClipHandle,
    pub time: f32,
}

#[derive(Clone, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
pub struct AnimationBlendAssetRing2d {
    pub stops: Vec<AnimationBlendAssetStop1d>,
    pub time: f32,
}

#[derive(Clone, Reflect, Debug, Deref, DerefMut)]
#[reflect(Serialize, Deserialize)]
pub struct AnimationBlendAssetClipHandle(pub Handle<AnimationClip>);

pub enum AnimationBlend {
    Single {
        clip: AssetId<AnimationClip>,
    },
    Blend {
        blend: AssetId<AnimationBlendAsset>,
        time: AnimationBlendTime,
    },
}

pub enum AnimationBlendTime {
    Blend1d(f32),
    Blend2d(Vec2),
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

impl Plugin for AnimationControllersPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<AnimationBlendAsset>()
            .add_message::<AnimationsRetargetedEvent>()
            .add_systems(PreUpdate, retargeting::prepare_retargeting)
            .add_systems(Update, retargeting::retarget_animations)
            .add_systems(
                PostUpdate,
                (advance_transitions, expire_completed_transitions)
                    .before(bevy::animation::animate_targets)
                    .in_set(AnimationSystems),
            );
    }
}

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

impl Serialize for AnimationBlendAssetClipHandle {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        ().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for AnimationBlendAssetClipHandle {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Err(de::Error::custom("TODO"))
    }
}

impl AnimationBlend {
    fn asset_id(&self) -> AnimationAssetId {
        match *self {
            AnimationBlend::Single { clip } => AnimationAssetId::Single(clip),
            AnimationBlend::Blend { blend, time: _ } => AnimationAssetId::Blend(blend),
        }
    }
}

impl AnimationBlendAssetStop1d {
    pub fn new(clip: Handle<AnimationClip>, time: f32) -> Self {
        Self {
            clip: AnimationBlendAssetClipHandle(clip),
            time,
        }
    }
}

impl From<Handle<AnimationClip>> for AnimationBlendAssetClipHandle {
    fn from(clip: Handle<AnimationClip>) -> Self {
        AnimationBlendAssetClipHandle(clip)
    }
}

impl From<AssetId<AnimationClip>> for AnimationAssetId {
    fn from(clip_id: AssetId<AnimationClip>) -> Self {
        AnimationAssetId::Single(clip_id)
    }
}

impl From<AssetId<AnimationBlendAsset>> for AnimationAssetId {
    fn from(blend_id: AssetId<AnimationBlendAsset>) -> Self {
        AnimationAssetId::Blend(blend_id)
    }
}

impl From<f32> for AnimationBlendTime {
    fn from(time: f32) -> Self {
        AnimationBlendTime::Blend1d(time)
    }
}

impl From<Vec2> for AnimationBlendTime {
    fn from(time: Vec2) -> Self {
        AnimationBlendTime::Blend2d(time)
    }
}

impl From<AssetId<AnimationClip>> for AnimationBlend {
    fn from(clip: AssetId<AnimationClip>) -> Self {
        AnimationBlend::Single { clip }
    }
}
