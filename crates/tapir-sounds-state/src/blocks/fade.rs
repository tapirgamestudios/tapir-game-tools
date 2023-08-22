use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct Fade {
    amplitude: f64,
    offset: f64,
}

impl Default for Fade {
    fn default() -> Self {
        Self {
            amplitude: 1.0,
            offset: 0.0,
        }
    }
}

impl Fade {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Alter,
            name: "Fade".to_owned(),
        }
    }
}

impl BlockType for Fade {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Amplitude".into(), Input::Amplitude(self.amplitude)),
            ("Offset".into(), Input::Periods(self.offset)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Amplitude(new_amplitude)) => {
                self.amplitude = *new_amplitude;
            }
            (1, Input::Periods(new_offset)) => {
                self.offset = new_offset.clamp(0.0, 1.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        let length = input.len() as f64 * (1.0 - self.offset);
        let offset_start = (input.len() as f64 * self.offset) as usize;

        if self.amplitude > 0.0 {
            // start at amplitude and end at 0. But only start doing something once we get to offset
            input
                .iter()
                .enumerate()
                .map(|(i, value)| {
                    if i < offset_start {
                        *value * self.amplitude
                    } else {
                        let amount = self.amplitude * (1.0 - (i - offset_start) as f64 / length);
                        *value * amount
                    }
                })
                .collect()
        } else {
            let offset_start = input.len() - offset_start;

            // start at 0 and end at -amplitude. But only start doing something once we get to offset
            input
                .iter()
                .enumerate()
                .map(|(i, value)| {
                    if i > offset_start {
                        *value * -self.amplitude
                    } else {
                        let amount = -self.amplitude * (i as f64 / length);
                        *value * amount
                    }
                })
                .collect()
        }
    }
}
