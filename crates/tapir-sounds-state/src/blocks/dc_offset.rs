use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::BlockType;

#[derive(Clone)]
pub struct DcOffset {
    offset: f64,
}

impl Default for DcOffset {
    fn default() -> Self {
        Self { offset: 0.0 }
    }
}

impl DcOffset {
    pub fn name() -> super::BlockName {
        super::BlockName {
            category: super::BlockCategory::Alter,
            name: "DC Offset".to_owned(),
        }
    }
}

impl BlockType for DcOffset {
    fn name(&self) -> super::BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, super::Input)]> {
        vec![("New Offset".into(), super::Input::Amplitude(self.offset))].into()
    }

    fn set_input(&mut self, index: usize, value: &super::Input) {
        match (index, value) {
            (0, super::Input::Amplitude(new_offset)) => {
                self.offset = new_offset.clamp(-1.0, 1.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return input;
        }

        let avg: f64 = input.iter().fold(0.0, |curr, next| curr + *next) / (input.len() as f64);
        let amount_to_fix_by = avg - self.offset;

        input
            .iter()
            .map(|value| (value - amount_to_fix_by).clamp(-1.0, 1.0))
            .collect()
    }
}
