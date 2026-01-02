//! Manages playback of animations.

use bevy::{
    animation::{AnimationPlayer, RepeatAnimation, graph::AnimationNodeIndex},
    asset::Assets,
    ecs::{
        component::Component,
        system::{Query, Res},
    },
    log::{error, info, trace, warn},
    reflect::Reflect,
    time::Time,
};
use smallvec::SmallVec;
use std::time::Duration;

use crate::{
    AnimationBlend, AnimationBlendAsset, AnimationBlendAssetType, AnimationBlendTime,
    AnimationLayer,
    control::AnimationTransitionMode,
    interpolation::{self, InterpolationResult},
};

/// A fancier version of [`bevy::animation::AnimationTransitions`].
#[derive(Clone, Component, Reflect, Debug)]
pub struct PlayingAnimations {
    pub layers: Vec<LayerAnimations>,
}

#[derive(Clone, Default, Reflect, Debug)]
pub struct LayerAnimations {
    pub main_animation: Option<PlayingAnimation>,
    pub transitions: Vec<TransitioningAnimation>,
}

#[derive(Clone, Reflect, Debug)]
pub struct PlayingAnimation {
    pub nodes: SmallVec<[AnimationNodeIndex; 1]>,
    pub blend: AnimationBlend,
}

#[derive(Clone, Reflect, Debug)]
pub struct TransitioningAnimation {
    pub current_weight: f32,
    pub weight_decline_per_sec: f32,
    pub animation: AnimationNodeIndex,
}

impl PlayingAnimations {
    pub fn new(layer_count: u32) -> PlayingAnimations {
        PlayingAnimations {
            layers: (0..layer_count)
                .map(|_| LayerAnimations::default())
                .collect(),
        }
    }

    pub fn group_mut(&mut self, index: AnimationLayer) -> &mut LayerAnimations {
        &mut self.layers[index.0 as usize]
    }
}

impl LayerAnimations {
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
        action: AnimationTransitionMode,
        animation_blend_assets: &Assets<AnimationBlendAsset>,
    ) {
        match action {
            AnimationTransitionMode::ChangeAndRestart => {
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

            AnimationTransitionMode::ChangeTime => {
                self.change_animation_time(player, new_animation, repeat, animation_blend_assets)
            }

            AnimationTransitionMode::NoChange => {}
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
            self.transitions.push(TransitioningAnimation {
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
                } = interpolation::linearly_interpolate_data(stops, time);

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

                let weights =
                    interpolation::polar_bilinear_interpolate(rings, &new_animation.nodes, time);

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
                } = interpolation::linearly_interpolate_data(stops, new_time);

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

                let weights = interpolation::polar_bilinear_interpolate(
                    rings,
                    &new_animation.nodes,
                    new_time,
                );

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

/// A system that alters the weight of currently-playing transitions based on
/// the current time and decline amount.
pub fn advance_transitions(
    mut q_animations: Query<(&mut PlayingAnimations, &mut AnimationPlayer)>,
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
        for animation_group_controller in animation_controller.layers.iter_mut() {
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
            } = interpolation::linearly_interpolate_data(stops, time);

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

            let weights =
                interpolation::polar_bilinear_interpolate(rings, &main_animation.nodes, time);

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
    mut q_animations: Query<(&mut PlayingAnimations, &mut AnimationPlayer)>,
) {
    for (mut animation_controller, mut player) in q_animations.iter_mut() {
        for animation_group_controller in animation_controller.layers.iter_mut() {
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
