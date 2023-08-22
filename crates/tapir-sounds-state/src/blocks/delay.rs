use std::{borrow::Cow, iter, rc::Rc, sync::Arc};

use super::{BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct Delay {
    input_amplitude: f64,
    delay: f64,
}

impl Default for Delay {
    fn default() -> Self {
        Self {
            input_amplitude: 1.0,
            delay: 0.0,
        }
    }
}

impl Delay {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Alter,
            name: "Delay".to_owned(),
        }
    }
}

impl BlockType for Delay {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Input".into(), Input::Amplitude(self.input_amplitude)),
            ("Amount".into(), Input::Periods(self.delay)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Amplitude(input_amplitude)) => {
                self.input_amplitude = *input_amplitude;
            }
            (1, Input::Periods(new_delay)) => {
                self.delay = new_delay.clamp(0.0, 10.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        let silence_length = (global_frequency * self.delay) as usize;

        iter::repeat(0.0)
            .take(silence_length)
            .chain(input.iter().map(|value| *value * self.input_amplitude))
            .collect()
    }
}
