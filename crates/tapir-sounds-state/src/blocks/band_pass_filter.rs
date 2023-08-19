use std::{borrow::Cow, rc::Rc, sync::Arc};

use super::{BlockName, BlockType, Input};

#[derive(Clone)]
pub struct BandPassFilter {
    base_amplitude: f64,
    lower_bound: f64,
    upper_bound: f64,
}

impl Default for BandPassFilter {
    fn default() -> Self {
        Self {
            base_amplitude: 1.0,
            lower_bound: 0.0,
            upper_bound: 1.0,
        }
    }
}

impl BandPassFilter {
    pub fn name() -> BlockName {
        BlockName {
            category: super::BlockCategory::Alter,
            name: "Band pass filter".to_owned(),
        }
    }
}

impl BlockType for BandPassFilter {
    fn name(&self) -> BlockName {
        Self::name()
    }

    fn inputs(&self) -> Rc<[(Cow<'static, str>, Input)]> {
        vec![
            ("Amplitude".into(), Input::Amplitude(self.base_amplitude)),
            ("Lower bound".into(), Input::Periods(self.lower_bound)),
            ("Upper bound".into(), Input::Periods(self.upper_bound)),
        ]
        .into()
    }

    fn set_input(&mut self, index: usize, value: &Input) {
        match (index, value) {
            (0, Input::Amplitude(amplitude)) => {
                self.base_amplitude = amplitude.clamp(0.0, 1.0);
            }
            (1, Input::Periods(new_lower_bound)) => {
                self.lower_bound = new_lower_bound.clamp(0.0, self.upper_bound);
            }
            (2, Input::Periods(new_upper_bound)) => {
                self.upper_bound = new_upper_bound.clamp(self.lower_bound, 1.0);
            }
            _ => panic!("Invalid input {index} {value:?}"),
        }
    }

    fn calculate(&self, _global_frequency: f64, inputs: &[Option<Arc<[f64]>>]) -> Arc<[f64]> {
        let input = inputs[0].clone().unwrap_or(Arc::new([]));

        if input.is_empty() {
            return vec![].into();
        }

        let mut planner = realfft::RealFftPlanner::<f64>::new();
        let fft = planner.plan_fft_forward(input.len());

        let mut input_data = input.to_vec();
        let mut spectrum = fft.make_output_vec();

        fft.process(&mut input_data, &mut spectrum).unwrap();

        let lower_frequencies = (input.len() as f64 * self.lower_bound) as usize;
        let upper_frequencies = (input.len() as f64 * self.upper_bound) as usize;

        for (i, value) in spectrum.iter_mut().enumerate() {
            if i < lower_frequencies || i > upper_frequencies {
                *value = 0.0.into();
            }
        }

        let inv_fft = planner.plan_fft_inverse(input.len());

        let mut output_data = inv_fft.make_output_vec();
        inv_fft.process(&mut spectrum, &mut output_data).unwrap();

        let normalization_amount = 1.0 / (input.len() as f64) * self.base_amplitude;

        output_data
            .iter()
            .map(|value| value * normalization_amount)
            .collect()
    }
}
