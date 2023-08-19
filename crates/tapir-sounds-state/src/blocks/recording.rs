use std::{borrow::Cow, sync::Arc};

use crate::Input;

use super::{BlockCategory, BlockName, BlockType};

#[derive(Clone)]
pub struct Recording {
    data: Arc<[f64]>,
}

impl Default for Recording {
    fn default() -> Self {
        Self { data: Arc::new([]) }
    }
}

impl Recording {
    pub fn name() -> BlockName {
        BlockName {
            category: BlockCategory::Input,
            name: "Recording".into(),
        }
    }
}

impl BlockType for Recording {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> std::rc::Rc<[(Cow<'static, str>, Input)]> {
        vec![("Recording".into(), Input::Recording(self.data.clone()))].into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Recording(new_data)) => {
                self.data = new_data.clone();
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, _inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        self.data.clone()
    }
}
