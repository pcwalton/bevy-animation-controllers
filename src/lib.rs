//! Experimental animation controllers for Bevy.

use std::borrow::Cow;

use crate::retargeting::{AnimationAssetId, AnimationsRetargetedEvent};

use bevy::{
    animation::AnimationClip,
    app::{AnimationSystems, App, Plugin, PostUpdate, PreUpdate, Update},
    asset::{Asset, AssetApp, AssetId, Handle},
    ecs::schedule::IntoScheduleConfigs as _,
    math::Vec2,
    prelude::{Deref, DerefMut, ReflectDeserialize, ReflectSerialize},
    reflect::Reflect,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de};

pub mod control;
mod interpolation;
pub mod playback;
pub mod retargeting;

pub struct AnimationControllersPlugin;

/// A mask value.
///
/// For example, this could be used to animate a character's upper body and
/// lower body separately.
#[derive(Clone, Copy, Reflect, PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Deref, DerefMut)]
pub struct AnimationLayer(pub u32);

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

#[derive(Clone, Reflect, Debug, Deref, DerefMut)]
pub struct LabeledAnimationBlend {
    #[deref]
    pub blend: AnimationBlend,
    pub label: Cow<'static, str>,
}

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

impl Plugin for AnimationControllersPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<AnimationBlendAsset>()
            .add_message::<AnimationsRetargetedEvent>()
            .add_systems(PreUpdate, retargeting::prepare_retargeting)
            .add_systems(Update, retargeting::retarget_animations)
            .add_systems(
                PostUpdate,
                (
                    playback::advance_transitions,
                    playback::expire_completed_transitions,
                )
                    .before(bevy::animation::animate_targets)
                    .in_set(AnimationSystems),
            );
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

impl From<AnimationBlend> for LabeledAnimationBlend {
    fn from(blend: AnimationBlend) -> Self {
        LabeledAnimationBlend {
            label: match blend {
                AnimationBlend::Single { .. } => Cow::Borrowed("(single animation)"),
                AnimationBlend::Blend { .. } => Cow::Borrowed("(animation blend)"),
            },
            blend,
        }
    }
}

impl From<AssetId<AnimationClip>> for LabeledAnimationBlend {
    fn from(clip: AssetId<AnimationClip>) -> Self {
        LabeledAnimationBlend::from(AnimationBlend::from(clip))
    }
}
