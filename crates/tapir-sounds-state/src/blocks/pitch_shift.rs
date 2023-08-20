use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{BlockCategory, BlockName, BlockType, Input};

#[derive(Clone)]
pub struct PitchShift {
    shift_amount: f64,
    octaves: f64,
}

impl Default for PitchShift {
    fn default() -> Self {
        Self {
            shift_amount: 0.0,
            octaves: 0.0,
        }
    }
}

impl PitchShift {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Alter,
            name: "Pitch Shift".to_owned(),
        }
    }
}

impl BlockType for PitchShift {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Input".into(), Input::Input),
            (
                "Amount in range".into(),
                Input::Amplitude(self.shift_amount),
            ),
            ("Octaves".into(), Input::Periods(self.octaves)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Input) => {}
            (1, Input::Amplitude(new_target)) => {
                self.shift_amount = *new_target;
            }
            (2, Input::Periods(new_octaves)) => {
                self.octaves = *new_octaves;
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return input;
        }

        let shift_amounts = inputs[1].clone().unwrap_or(Arc::new([0.0]));

        let mut index = 0.0f64;
        let mut ret = Vec::new();

        while (index as usize) < input.len() {
            let i = index as usize;

            let shift_amount =
                shift_amounts[i % shift_amounts.len()] + self.shift_amount + self.octaves;

            let actual_modification = 2f64.powf(shift_amount);

            ret.push(input[i]);

            index += actual_modification;
        }

        ret.into()
    }
}
