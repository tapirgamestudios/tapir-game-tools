use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct Combine {
    num_inputs: usize,
    loop_shortest: bool,
    inputs: Vec<f64>,
}

impl Default for Combine {
    fn default() -> Self {
        Self {
            num_inputs: 0,
            loop_shortest: true,
            inputs: vec![],
        }
    }
}

impl Combine {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Combine,
            name: "Combine".to_owned(),
        }
    }
}

impl BlockType for Combine {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        [
            (
                "Number of inputs".into(),
                Input::Periods(self.num_inputs as f64),
            ),
            ("Loop shortest".into(), Input::Toggle(self.loop_shortest)),
        ]
        .into_iter()
        .chain(
            self.inputs
                .iter()
                .enumerate()
                .map(|(i, input)| (format!("Input #{}", i + 1).into(), Input::Amplitude(*input))),
        )
        .collect()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Periods(new_number_of_inputs)) => {
                self.num_inputs = *new_number_of_inputs as usize;
                self.inputs.resize(self.num_inputs, 1.0);
            }
            (1, Input::Toggle(loop_shortest)) => {
                self.loop_shortest = *loop_shortest;
            }
            (i, Input::Amplitude(new_input)) => {
                if let Some(value) = self.inputs.get_mut(i - 2) {
                    *value = *new_input;
                }
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let length = if self.loop_shortest {
            inputs.iter().fold(0, |curr, next| {
                next.as_ref()
                    .map(|next| next.len())
                    .unwrap_or(curr)
                    .max(curr)
            })
        } else {
            inputs.iter().fold(usize::MAX, |curr, next| {
                next.as_ref()
                    .map(|next| next.len())
                    .unwrap_or(curr)
                    .min(curr)
            })
        };

        if length == 0 || length == usize::MAX {
            return Arc::new([]);
        }

        let mut result = Vec::with_capacity(length);

        for i in 0..length {
            result.push(
                inputs.iter().skip(2).zip(self.inputs.iter()).fold(
                    0.0,
                    |curr, (next_input, amplitude)| {
                        curr + next_input
                            .as_ref()
                            .map(|next_input| next_input[i % next_input.len()])
                            .unwrap_or(0.0)
                            * *amplitude
                    },
                ) / self.num_inputs as f64,
            );
        }

        result.into()
    }
}
