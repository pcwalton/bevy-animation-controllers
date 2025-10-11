//! Animation retargeting.

use arrayvec::ArrayVec;
use bevy::{
    animation::{
        AnimationClip, AnimationPlayer, AnimationTarget, AnimationTargetId, RepeatAnimation,
        graph::{AnimationGraph, AnimationGraphHandle, AnimationNodeIndex},
    },
    asset::{AssetServer, Assets, Handle},
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::Children,
        message::{Message, MessageWriter},
        name::Name,
        query::With,
        system::{Commands, Query, Res, ResMut},
    },
    gltf::Gltf,
    log::{debug, error},
    platform::collections::HashMap,
    prelude::{Deref, DerefMut},
    reflect::Reflect,
    scene::SceneRoot,
};
use smallvec::{SmallVec, smallvec};
use std::iter;

use crate::{
    AnimationBlend2d, AnimationGroup, AnimationTag, AnimationTagHandle, INLINE_ANIMATION_BLENDS,
    INLINE_ANIMATION_RINGS, math::InterpolationPoint1d,
};

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

impl InterpolationPoint1d for RetargetedAnimationBlendStop1d {
    fn time(&self) -> f32 {
        self.time
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
