use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::BlockType;

#[derive(Clone)]
pub struct Amplify {
    amplitude: f64,
}

impl Default for Amplify {
    fn default() -> Self {
        Self { amplitude: 1.0 }
    }
}

impl Amplify {
    pub fn name() -> super::BlockName {
        super::BlockName {
            category: super::BlockCategory::Alter,
            name: "Amplify".to_owned(),
        }
    }
}

impl BlockType for Amplify {
    fn name(&self) -> super::BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, super::Input)]> {
        vec![(
            "New Maximum".into(),
            super::Input::Amplitude(self.amplitude),
        )]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &super::Input) {
        match (index, value) {
            (0, super::Input::Amplitude(new_amplitude)) => {
                self.amplitude = new_amplitude.clamp(0.0, 1.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return input;
        }

        let max = input.iter().fold(0.0f64, |max, next| max.max(next.abs()));
        let factor = self.amplitude / max;

        input.iter().map(|value| value * factor).collect()
    }
}
