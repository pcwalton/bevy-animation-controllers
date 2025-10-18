//! Blendtree interpolation algorithms.

use bevy::{animation::graph::AnimationNodeIndex, math::Vec2};
use smallvec::{SmallVec, smallvec};
use std::{cmp::Ordering, f32::consts::PI};

use crate::{AnimationBlendAssetRing2d, AnimationBlendAssetStop1d, LinearInterpolationWeights};

/// The number of animation blends we store inline in a [`SmallVec`].
const INLINE_ANIMATION_BLENDS: usize = 5;

pub struct InterpolationResult {
    pub prev_index: usize,
    pub next_index: Option<usize>,
    pub weight: f32,
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

pub fn linearly_interpolate_data(
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
