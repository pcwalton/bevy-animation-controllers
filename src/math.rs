//! General math routines.

pub trait InterpolationPoint1d {
    fn time(&self) -> f32;
}

pub struct InterpolationResult {
    pub prev_index: usize,
    pub next_index: Option<usize>,
    pub weight: f32,
}

pub fn linearly_interpolate_data<P>(points: &[P], time: f32) -> InterpolationResult
where
    P: InterpolationPoint1d,
{
    debug_assert_ne!(points.len(), 0);

    let mut next_point_index = None;
    for (point_index, point) in points.iter().enumerate() {
        if time < point.time() {
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
                prev_index: points.len() - 1,
                next_index: None,
                weight: 0.0,
            }
        }
        Some(next_index) => {
            // In between two stops.
            let prev_index = next_index - 1;
            let prev_time = points[prev_index].time();
            let next_time = points[next_index].time();
            let weight = (time - prev_time) / (next_time - prev_time);
            InterpolationResult {
                prev_index: next_index - 1,
                next_index: Some(next_index),
                weight,
            }
        }
    }
}
