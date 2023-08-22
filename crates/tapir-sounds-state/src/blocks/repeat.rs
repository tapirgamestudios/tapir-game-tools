use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct Repeat {
    num_repeats: f64,
    is_seconds: bool,
}

impl Default for Repeat {
    fn default() -> Self {
        Self {
            num_repeats: 1.0,
            is_seconds: true,
        }
    }
}

impl Repeat {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Alter,
            name: "Repeat".to_owned(),
        }
    }
}

impl BlockType for Repeat {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Input".into(), Input::Input),
            ("Count".into(), Input::Periods(self.num_repeats)),
            ("Seconds".into(), Input::Toggle(self.is_seconds)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Input) => {}
            (1, Input::Periods(num_repeats)) => {
                self.num_repeats = if self.is_seconds {
                    num_repeats.clamp(0.0, 10.0)
                } else {
                    num_repeats.clamp(0.0, 10_000.0)
                }
            }
            (2, Input::Toggle(is_seconds)) => {
                self.is_seconds = *is_seconds;
                self.num_repeats = if self.is_seconds {
                    self.num_repeats.clamp(0.0, 10.0)
                } else {
                    self.num_repeats.clamp(0.0, 10_000.0)
                }
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return input;
        }

        let max_total_length = (global_frequency * 10.0) as usize;

        let total_length = (if self.is_seconds {
            global_frequency * self.num_repeats
        } else {
            input.len() as f64 * self.num_repeats
        } as usize)
            .min(max_total_length);

        let mut output = Vec::with_capacity(total_length);
        for i in 0..total_length {
            output.push(input[i % input.len()]);
        }

        output.into()
    }
}
