use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::BlockType;

#[derive(Clone)]
pub struct Cut {
    min: f64,
    max: f64,
}

impl Default for Cut {
    fn default() -> Self {
        Self {
            min: -1.0,
            max: 1.0,
        }
    }
}

impl Cut {
    pub fn name() -> super::BlockName {
        super::BlockName {
            category: super::BlockCategory::Alter,
            name: "Cut".to_owned(),
        }
    }
}

impl BlockType for Cut {
    fn name(&self) -> super::BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, super::Input)]> {
        vec![
            ("New Minimum".into(), super::Input::Amplitude(self.min)),
            ("Input".into(), super::Input::Input),
            ("New Maximum".into(), super::Input::Amplitude(self.max)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &super::Input) {
        match (index, value) {
            (0, super::Input::Amplitude(new_minimum)) => {
                self.min = new_minimum.clamp(-1.0, self.max);
            }
            (1, super::Input::Input) => {}
            (2, super::Input::Amplitude(new_maximum)) => {
                self.max = new_maximum.clamp(self.min, 1.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[1].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return input;
        }

        let min = inputs[0].clone().unwrap_or(Arc::new([-1.0]));
        let max = inputs[2].clone().unwrap_or(Arc::new([1.0]));

        input
            .iter()
            .enumerate()
            .map(|(i, value)| {
                let min = min[i % min.len()].max(self.min);
                let max = max[i % max.len()].min(self.max);

                if min > max {
                    return (min + max) / 2.0;
                }

                value.clamp(min, max)
            })
            .collect()
    }
}
