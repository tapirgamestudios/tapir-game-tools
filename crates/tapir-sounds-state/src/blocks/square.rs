use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{stretch_frequency_shift, BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct Square {
    periods: f64,
    base_frequency: f64,
    base_amplitude: f64,
    offset: f64,
    duty_cycle: f64,
}

impl Square {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Fundamental,
            name: "Square".to_owned(),
        }
    }

    fn value_at(&self, x: f64) -> f64 {
        if x < self.duty_cycle {
            -1.0
        } else {
            1.0
        }
    }
}

impl Default for Square {
    fn default() -> Self {
        Self {
            base_frequency: 256.0,
            base_amplitude: 0.5,
            periods: 1.0,
            offset: 0.0,
            duty_cycle: 0.5,
        }
    }
}

impl BlockType for Square {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Frequency".into(), Input::Frequency(self.base_frequency)),
            ("Amplitude".into(), Input::Amplitude(self.base_amplitude)),
            ("Periods".into(), Input::Periods(self.periods)),
            ("Offset".into(), Input::Periods(self.offset)),
            ("Duty Cycle".into(), Input::Periods(self.duty_cycle)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Frequency(new_frequency)) => {
                if *new_frequency != 0.0 {
                    self.base_frequency = *new_frequency;
                }
            }
            (1, Input::Amplitude(new_amplitude)) => {
                self.base_amplitude = *new_amplitude;
            }
            (2, Input::Periods(new_periods)) => {
                self.periods = *new_periods;
            }
            (3, Input::Periods(new_offset)) => {
                self.offset = new_offset.clamp(0.0, 1.0);
            }
            (4, Input::Periods(new_duty_cycle)) => {
                self.duty_cycle = new_duty_cycle.clamp(0.0, 1.0);
            }
            (name, value) => panic!("Invalid input {name} with value {value:?}"),
        }
    }

    fn calculate(&self, global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let periods = if self.periods == 0.0 {
            1.0
        } else {
            self.periods
        };

        let period_length = (global_frequency / self.base_frequency).ceil();
        let length = (period_length * periods) as usize;

        let mut ret = Vec::with_capacity(length);
        for i in 0..length {
            let frequency_at_i = self.base_frequency
                * stretch_frequency_shift(
                    inputs[0]
                        .clone()
                        .map(|frequency_input| frequency_input[i % frequency_input.len()])
                        .unwrap_or(0.0),
                )
                .clamp(0.1, 10_000.0);

            let amplitude_at_i = (self.base_amplitude
                * inputs[1]
                    .clone()
                    .map(|amplitude_input| amplitude_input[i % amplitude_input.len()])
                    .unwrap_or(1.0))
            .clamp(-1.0, 1.0);

            let period_length_at_i = global_frequency / frequency_at_i;

            ret.push(
                self.value_at((i as f64 / period_length_at_i + self.offset).fract())
                    * amplitude_at_i,
            );
        }

        ret.into()
    }
}
