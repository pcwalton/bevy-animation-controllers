//! Animated characters.

use bevy::{
    animation::AnimationPlayer,
    ecs::{
        component::{Component, Mutable},
        entity::Entity,
        hierarchy::Children,
        system::{Query, StaticSystemParam, SystemParam},
    },
    log::{debug, error, warn},
};
use std::{any, time::Duration};

use crate::{
    retargeting::{RetargetedAnimation, RetargetedAnimationNodes, RetargetedAnimations}, AnimationAction, AnimationBlend, AnimationController, AnimationGroup, PlayingAnimation
};

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
