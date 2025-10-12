//! Experimental animation controllers for Bevy.

use crate::retargeting::{AnimationAssetId, AnimationsRetargetedEvent};

use bevy::{
    animation::{AnimationClip, AnimationPlayer, RepeatAnimation, graph::AnimationNodeIndex},
    app::{AnimationSystems, App, Plugin, PostUpdate, PreUpdate, Update},
    asset::{Asset, AssetApp, AssetId, Assets, Handle},
    ecs::{component::Component, schedule::IntoScheduleConfigs as _, system::Res},
    log::{error, info, trace, warn},
    math::Vec2,
    prelude::{Deref, DerefMut, Query, ReflectDeserialize, ReflectSerialize},
    reflect::Reflect,
    time::Time,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};
use smallvec::{SmallVec, smallvec};
use std::{cmp::Ordering, f32::consts::PI, time::Duration};

pub mod characters;
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

#[derive(Clone, Reflect, Debug)]
pub struct PlayingAnimation {
    pub nodes: SmallVec<[AnimationNodeIndex; 1]>,
    pub blend: AnimationBlend,
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

#[derive(Clone, Reflect, Debug)]
pub enum AnimationBlend {
    Single {
        clip: AssetId<AnimationClip>,
    },
    Blend {
        blend: AssetId<AnimationBlendAsset>,
        time: AnimationBlendTime,
    },
}

#[derive(Clone, Copy, PartialEq, Reflect, Debug)]
pub enum AnimationBlendTime {
    Blend1d(f32),
    Blend2d(Vec2),
}

#[derive(Debug)]
struct LinearInterpolationWeights {
    prev_stop_index: usize,
    next_stop_index: usize,
    time: f32,
}

pub struct InterpolationResult {
    pub prev_index: usize,
    pub next_index: Option<usize>,
    pub weight: f32,
}

/// The number of animation blends we store inline in a [`SmallVec`].
const INLINE_ANIMATION_BLENDS: usize = 5;

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
        animation_blend_assets: &Assets<AnimationBlendAsset>,
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
                    new_animation
                        .nodes
                        .iter()
                        .all(|new_playing_animation_node| {
                            *new_playing_animation_node != transition.animation
                        })
                });

                self.start_playing_new_animation(
                    player,
                    new_animation,
                    repeat,
                    animation_blend_assets,
                );
            }

            AnimationAction::ChangeTime => {
                self.change_animation_time(player, new_animation, repeat, animation_blend_assets)
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

        for old_playing_animation_node in old_playing_animation.nodes.iter() {
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
        animation_blend_assets: &Assets<AnimationBlendAsset>,
    ) {
        match new_animation.blend {
            AnimationBlend::Single { .. } => {
                player.start(new_animation.nodes[0]).set_repeat(repeat);
            }

            AnimationBlend::Blend {
                blend: blend_id,
                time: AnimationBlendTime::Blend1d(time),
            } => {
                let Some(blend) = animation_blend_assets.get(blend_id) else {
                    warn!(
                        "Couldn't start playing new 1D blend {:?} because the blend asset didn't \
                         exist",
                        blend_id
                    );
                    return;
                };
                let AnimationBlendAssetType::Blend1d { ref stops } = blend.blend_type else {
                    warn!(
                        "Couldn't start playing new 1D blend {:?} because the blend asset wasn't a \
                         1D blend",
                        blend_id
                    );
                    return;
                };

                let InterpolationResult {
                    prev_index,
                    next_index,
                    weight,
                } = linearly_interpolate_data(stops, time);

                trace!(
                    "Interpolating between stop {:?} and {:?}: time {:?} prev time {:?} \
                     next time {:?} next weight {:?}",
                    prev_index,
                    next_index,
                    time,
                    stops[prev_index].time,
                    next_index.map(|next_index| stops[next_index].time),
                    weight
                );

                player
                    .start(new_animation.nodes[prev_index])
                    .set_repeat(repeat)
                    .set_weight(1.0 - weight);

                if let Some(next_index) = next_index {
                    player
                        .start(new_animation.nodes[next_index])
                        .set_repeat(repeat)
                        .set_weight(weight);
                }
            }

            AnimationBlend::Blend {
                blend: blend_id,
                time: AnimationBlendTime::Blend2d(time),
            } => {
                let Some(blend) = animation_blend_assets.get(blend_id) else {
                    warn!(
                        "Couldn't start playing new 2D blend {:?} because the blend asset didn't \
                         exist",
                        blend_id
                    );
                    return;
                };
                let AnimationBlendAssetType::Blend2d {
                    ref rings,
                    center: _,
                } = blend.blend_type
                else {
                    warn!(
                        "Couldn't start playing new 2D blend {:?} because the blend asset wasn't a \
                         2D blend",
                        blend_id
                    );
                    return;
                };

                let weights = polar_bilinear_interpolate(rings, &new_animation.nodes, time);

                for (&node, weight) in new_animation.nodes.iter().zip(weights.iter()) {
                    player.start(node).set_repeat(repeat).set_weight(*weight);
                }
            }
        }
    }

    fn change_animation_time(
        &mut self,
        player: &mut AnimationPlayer,
        new_animation: PlayingAnimation,
        repeat: RepeatAnimation,
        animation_blend_assets: &Assets<AnimationBlendAsset>,
    ) {
        let Some(ref mut main_animation) = self.main_animation else {
            warn!("Attempted to change animation time, but no main animation was playing");
            return;
        };

        match (new_animation.blend, &mut main_animation.blend) {
            (AnimationBlend::Single { .. }, _) => {
                error!(
                    "Attempted to change the time of a non-blend: {:?}",
                    new_animation.nodes[0]
                );
            }

            (
                AnimationBlend::Blend {
                    blend: new_blend,
                    time: AnimationBlendTime::Blend1d(new_time),
                },
                &mut AnimationBlend::Blend {
                    blend: ref mut playing_blend,
                    time: AnimationBlendTime::Blend1d(ref mut playing_time),
                },
            ) => {
                if new_blend != *playing_blend {
                    warn!("Attempted to change the time of a 1D blend that wasn't playing");
                    return;
                }

                let Some(blend) = animation_blend_assets.get(new_blend) else {
                    warn!(
                        "Couldn't start playing new 1D blend {:?} because the blend asset didn't \
                         exist",
                        new_blend
                    );
                    return;
                };
                let AnimationBlendAssetType::Blend1d { ref stops } = blend.blend_type else {
                    warn!(
                        "Couldn't start playing new 1D blend {:?} because the blend asset wasn't a \
                         1D blend",
                        new_blend
                    );
                    return;
                };

                *playing_time = new_time;

                let InterpolationResult {
                    prev_index,
                    next_index,
                    weight,
                } = linearly_interpolate_data(stops, new_time);

                trace!(
                    "Interpolating between stop {:?} and {:?}: time {:?} prev time {:?} \
                     next time {:?} next weight {:?}",
                    prev_index,
                    next_index,
                    new_time,
                    stops[prev_index].time,
                    next_index.map(|next_index| stops[next_index].time),
                    weight
                );

                player
                    .play(new_animation.nodes[prev_index])
                    .set_repeat(repeat)
                    .set_weight(1.0 - weight);

                if let Some(next_index) = next_index {
                    player
                        .play(new_animation.nodes[next_index])
                        .set_repeat(repeat)
                        .set_weight(weight);
                }
            }

            (
                AnimationBlend::Blend {
                    blend: _,
                    time: AnimationBlendTime::Blend1d(_),
                },
                _,
            ) => {
                warn!("Attempted to change the time of a 1D blend, but a 1D blend wasn't playing");
            }

            (
                AnimationBlend::Blend {
                    blend: new_blend,
                    time: AnimationBlendTime::Blend2d(new_time),
                },
                &mut AnimationBlend::Blend {
                    blend: ref mut playing_blend,
                    time: AnimationBlendTime::Blend2d(ref mut playing_time),
                },
            ) => {
                if new_blend != *playing_blend {
                    warn!("Attempted to change the time of a 2D blend that wasn't playing");
                    return;
                }

                let Some(blend) = animation_blend_assets.get(new_blend) else {
                    warn!(
                        "Couldn't start playing new 2D blend {:?} because the blend asset didn't \
                         exist",
                        new_blend
                    );
                    return;
                };
                let AnimationBlendAssetType::Blend2d {
                    ref rings,
                    center: _,
                } = blend.blend_type
                else {
                    warn!(
                        "Couldn't start playing new 2D blend {:?} because the blend asset wasn't a \
                         2D blend",
                        new_blend
                    );
                    return;
                };

                *playing_time = new_time;

                let weights = polar_bilinear_interpolate(rings, &new_animation.nodes, new_time);

                for (node, weight) in new_animation.nodes.iter().zip(weights.iter()) {
                    player.play(*node).set_repeat(repeat).set_weight(*weight);
                }
            }

            (
                AnimationBlend::Blend {
                    blend: _,
                    time: AnimationBlendTime::Blend2d(_),
                },
                _,
            ) => {
                warn!("Attempted to change the time of a 2D blend, but a 2D blend wasn't playing");
            }
        }
    }
}

pub fn polar_bilinear_interpolate(
    rings: &[AnimationBlendAssetRing2d],
    nodes: &[AnimationNodeIndex],
    time: Vec2,
) -> SmallVec<[f32; INLINE_ANIMATION_BLENDS]> {
    // FIXME(pcwalton): This could be a fixed array of length 2.
    let mut first_stop_in_each_ring = Vec::with_capacity(rings.len() + 1);
    first_stop_in_each_ring.push(1);
    for ring in rings {
        let first_stop_in_this_ring = *first_stop_in_each_ring.last().unwrap() + ring.stops.len();
        first_stop_in_each_ring.push(first_stop_in_this_ring);
    }

    let mut next_ring_index = None;
    for (ring_index, ring) in rings.iter().enumerate() {
        if time.x < ring.time {
            next_ring_index = Some(ring_index);
            break;
        }
    }
    let next_ring_index = next_ring_index.unwrap_or(rings.len());

    let next_weights = rings.get(next_ring_index).map(|ring| {
        (
            linear_interpolate_ring(ring, time.y),
            first_stop_in_each_ring[next_ring_index],
        )
    });

    let prev_weights = if next_ring_index > 0 {
        Some((
            linear_interpolate_ring(&rings[next_ring_index - 1], time.y),
            first_stop_in_each_ring[next_ring_index - 1],
        ))
    } else {
        None
    };

    let mut weights = smallvec![0.0; nodes.len()];

    match (prev_weights, next_weights) {
        (None, None) => weights[0] = 1.0,
        (Some((prev_weights, first_stop_in_prev_ring)), None) => {
            weights[first_stop_in_prev_ring + prev_weights.prev_stop_index] =
                1.0 - prev_weights.time;
            weights[first_stop_in_prev_ring + prev_weights.next_stop_index] = prev_weights.time;
        }
        (None, Some((next_weights, first_stop_in_next_ring))) => {
            let radius_time = time.x / rings[0].time;
            weights[0] = radius_time;
            weights[first_stop_in_next_ring + next_weights.prev_stop_index] =
                (1.0 - radius_time) * (1.0 - next_weights.time);
            weights[first_stop_in_next_ring + next_weights.next_stop_index] =
                (1.0 - radius_time) * next_weights.time;
        }
        (
            Some((prev_weights, first_stop_in_prev_ring)),
            Some((next_weights, first_stop_in_next_ring)),
        ) => {
            let prev_ring_index = next_ring_index - 1;
            let radius_time = linstep(
                rings[prev_ring_index].time,
                rings[next_ring_index].time,
                time.x,
            );
            weights[first_stop_in_prev_ring + prev_weights.prev_stop_index] =
                radius_time * (1.0 - prev_weights.time);
            weights[first_stop_in_prev_ring + prev_weights.next_stop_index] =
                radius_time * prev_weights.time;
            weights[first_stop_in_next_ring + next_weights.prev_stop_index] =
                (1.0 - radius_time) * (1.0 - next_weights.time);
            weights[first_stop_in_next_ring + next_weights.next_stop_index] =
                (1.0 - radius_time) * next_weights.time;
        }
    }

    weights
}

fn linear_interpolate_ring(
    ring: &AnimationBlendAssetRing2d,
    time: f32,
) -> LinearInterpolationWeights {
    debug_assert!(!ring.stops.is_empty());

    if ring.stops.len() == 1 {
        return LinearInterpolationWeights {
            prev_stop_index: 0,
            next_stop_index: 1,
            time: 0.0,
        };
    }

    let time = time.rem_euclid(2.0 * PI);

    let mut next_stop_index = match ring
        .stops
        .binary_search_by(|stop| stop.time.partial_cmp(&time).unwrap_or(Ordering::Less))
    {
        Ok(next_stop_index) => next_stop_index,
        Err(next_stop_index) => next_stop_index,
    };
    if next_stop_index == ring.stops.len() {
        next_stop_index = 0;
    }
    let prev_stop_index = if next_stop_index == 0 {
        ring.stops.len() - 1
    } else {
        next_stop_index - 1
    };

    let prev_time = ring.stops[prev_stop_index].time;
    let mut next_time = ring.stops[next_stop_index].time;
    if next_time < prev_time {
        next_time += 2.0 * PI;
    }

    let time = linstep(prev_time, next_time, time);

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
    animation_blend_assets: Res<Assets<AnimationBlendAsset>>,
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
                distribute_weight(
                    &mut player,
                    main_animation,
                    remaining_weight,
                    &animation_blend_assets,
                );
            }
        }
    }
}

fn distribute_weight(
    player: &mut AnimationPlayer,
    main_animation: &PlayingAnimation,
    remaining_weight: f32,
    animation_blend_assets: &Assets<AnimationBlendAsset>,
) {
    match main_animation.blend {
        AnimationBlend::Single { .. } => {
            if let Some(ref mut animation) = player.animation_mut(main_animation.nodes[0]) {
                animation.set_weight(remaining_weight);
            }
        }

        AnimationBlend::Blend {
            blend: blend_id,
            time: AnimationBlendTime::Blend1d(time),
        } => {
            let Some(blend) = animation_blend_assets.get(blend_id) else {
                warn!(
                    "Couldn't distribute weight for 1D blend {:?} because the blend asset didn't \
                         exist",
                    blend_id
                );
                return;
            };
            let AnimationBlendAssetType::Blend1d { ref stops } = blend.blend_type else {
                warn!(
                    "Couldn't distribute weight for 1D blend {:?} because the blend asset wasn't a \
                         1D blend",
                    blend_id
                );
                return;
            };

            // Evaluate blend and distribute remaining weight.

            let InterpolationResult {
                prev_index,
                next_index,
                weight: interpolated_weight,
            } = linearly_interpolate_data(stops, time);

            trace!(
                "Distributing weight: Interpolating between stop {:?} and {:?}: time {:?} prev \
                    time {:?} next time {:?} next weight {:?}",
                prev_index,
                next_index,
                time,
                stops[prev_index].time,
                next_index.map(|next_index| stops[next_index].time),
                interpolated_weight
            );

            for (blend_stop_index, node) in main_animation.nodes.iter().enumerate() {
                let Some(ref mut animation) = player.animation_mut(*node) else {
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

        AnimationBlend::Blend {
            blend: blend_id,
            time: AnimationBlendTime::Blend2d(time),
        } => {
            let Some(blend) = animation_blend_assets.get(blend_id) else {
                warn!(
                    "Couldn't distribute weight for 2D blend {:?} because the blend asset didn't \
                         exist",
                    blend_id
                );
                return;
            };
            let AnimationBlendAssetType::Blend2d {
                ref rings,
                center: _,
            } = blend.blend_type
            else {
                warn!(
                    "Couldn't distribute weight for 2D blend {:?} because the blend asset wasn't a \
                     2D blend",
                    blend_id
                );
                return;
            };

            let weights = polar_bilinear_interpolate(rings, &main_animation.nodes, time);

            for (node, interpolated_weight) in main_animation.nodes.iter().zip(weights.iter()) {
                if let Some(ref mut animation) = player.animation_mut(*node) {
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

fn linearly_interpolate_data(
    stops: &[AnimationBlendAssetStop1d],
    time: f32,
) -> InterpolationResult {
    debug_assert!(!stops.is_empty());

    let mut next_point_index = None;
    for (point_index, point) in stops.iter().enumerate() {
        if time < point.time {
            next_point_index = Some(point_index);
            break;
        }
    }

    match next_point_index {
        Some(0) => {
            // Before first point.
            InterpolationResult {
                prev_index: 0,
                next_index: None,
                weight: 0.0,
            }
        }
        None => {
            // After last point.
            InterpolationResult {
                prev_index: stops.len() - 1,
                next_index: None,
                weight: 0.0,
            }
        }
        Some(next_index) => {
            // In between two stops.
            let prev_index = next_index - 1;
            let prev_time = stops[prev_index].time;
            let next_time = stops[next_index].time;
            let weight = linstep(prev_time, next_time, time);
            InterpolationResult {
                prev_index: next_index - 1,
                next_index: Some(next_index),
                weight,
            }
        }
    }
}

pub fn linstep(edge_0: f32, edge_1: f32, x: f32) -> f32 {
    (x - edge_0) / (edge_1 - edge_0)
}
