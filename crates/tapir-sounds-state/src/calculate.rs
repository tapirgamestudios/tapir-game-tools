use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
    thread::{self, JoinHandle},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CalculationId(crate::Id);

#[derive(Clone)]
pub struct Calculation {
    results: Arc<HashMap<crate::Id, Vec<f64>>>,
    id: CalculationId,
}

impl Calculation {
    pub fn for_block(&self, block_id: crate::Id) -> Option<&Vec<f64>> {
        self.results.get(&block_id)
    }

    pub fn id(&self) -> CalculationId {
        self.id
    }
}

pub struct Calculator {
    previous_results: Arc<RwLock<Option<Calculation>>>,
    worker_thread: Option<JoinHandle<()>>,
}

impl Default for Calculator {
    fn default() -> Self {
        Self {
            previous_results: Arc::new(RwLock::new(None)),
            worker_thread: None,
        }
    }
}

impl Calculator {
    pub fn calculate(&mut self, state: &crate::State) -> bool {
        if let Some(worker) = &self.worker_thread {
            if worker.is_finished() {
                self.worker_thread.take().unwrap().join().unwrap();
            } else {
                return false;
            }
        }

        let previous_result = self.previous_results.clone();
        let state = state.clone();

        self.worker_thread = Some(thread::spawn(move || {
            let calculated_result = calculate(state);
            *previous_result.write().unwrap() = Some(calculated_result);
        }));

        true
    }

    pub fn is_calculating(&self) -> bool {
        self.worker_thread
            .as_ref()
            .is_some_and(|worker| !worker.is_finished())
    }

    pub fn results(&self) -> Option<Calculation> {
        self.previous_results.read().unwrap().clone()
    }
}

fn calculate(state: crate::State) -> Calculation {
    let results = state.calculate();

    Calculation {
        results: Arc::new(results),
        id: CalculationId(crate::Id::new()),
    }
}
