//! Animated characters.

use bevy::{
    animation::AnimationPlayer,
    asset::Assets,
    ecs::{
        component::{Component, Mutable},
        entity::Entity,
        hierarchy::Children,
        system::{Query, Res, StaticSystemParam, SystemParam},
    },
    log::{debug, error, warn},
    reflect::Reflect,
};
use std::{any, time::Duration};

use crate::{
    AnimationBlend, AnimationBlendAsset, AnimationLayer,
    playback::{PlayingAnimation, PlayingAnimations},
    retargeting::{RetargetedAnimation, RetargetedAnimations},
};

pub trait AnimationControl: Component {
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
        group: AnimationLayer,
        new_state: &Self::AnimationState,
    ) -> AnimationTransitionMode;
    fn animation_for_state(
        group: AnimationLayer,
        state: &Self::AnimationState,
        param: &StaticSystemParam<'_, '_, Self::SystemParam>,
    ) -> (Option<AnimationBlend>, Duration);
    fn set_current_animation_state(&mut self, new_state: &Self::AnimationState);
}

/// How a character animation transition should occur.
#[derive(Clone, Copy, PartialEq, Reflect, Debug)]
pub enum AnimationTransitionMode {
    /// Keep the animation playing as is.
    NoChange,
    /// Change the time of the current 1D or 2D blend animation.
    ChangeTime,
    /// Change the animation, and restart it.
    ChangeAndRestart,
}

pub fn update_animation_controllers<A: AnimationControl + Component<Mutability = Mutable>>(
    mut q_characters: Query<(Entity, &mut A, &Children)>,
    mut q_rigs: Query<(
        &mut PlayingAnimations,
        &mut AnimationPlayer,
        &RetargetedAnimations,
    )>,
    animation_blend_assets: Res<Assets<AnimationBlendAsset>>,
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
            let group = AnimationLayer(group);
            let animation_action =
                animated_character.compute_animation_action(group, &new_animation_state);
            if animation_action == AnimationTransitionMode::NoChange {
                continue;
            }
            any_dirty = true;

            let (Some(new_animation_blend), animation_transition_time) =
                A::animation_for_state(group, &new_animation_state, &param)
            else {
                continue;
            };
            let Some(RetargetedAnimation { nodes, repeat }) =
                retargeted_animations.get(&(group, new_animation_blend.asset_id()))
            else {
                warn!(
                    "Couldn't find animation for {:?}, group {:?}, tag {:?}",
                    any::type_name::<A>(),
                    group,
                    new_animation_blend.asset_id()
                );
                continue;
            };

            debug!(
                "Changing animation for {:?}, group {:?}",
                any::type_name::<A>(),
                group
            );

            let playing_animation = PlayingAnimation {
                blend: new_animation_blend,
                // FIXME(pcwalton): We shouldn't clone this.
                nodes: nodes.clone(),
            };

            animation_controller.group_mut(group).play(
                &mut animation_player,
                playing_animation,
                animation_transition_time,
                *repeat,
                animation_action,
                &animation_blend_assets,
            )
        }

        if any_dirty {
            animated_character.set_current_animation_state(&new_animation_state);
        }
    }
}
