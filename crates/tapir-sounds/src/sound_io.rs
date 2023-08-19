use std::sync::Arc;

use cpal::traits::{DeviceTrait, HostTrait};

use crate::audio;

pub struct SoundIo {
    audio: Arc<audio::Audio>,
    stream: Option<cpal::Stream>,
}

impl SoundIo {
    pub fn new(audio: Arc<audio::Audio>) -> Self {
        Self {
            audio,
            stream: None,
        }
    }

    pub fn start_sound(&mut self) -> anyhow::Result<()> {
        let host = cpal::default_host();
        let output_device = host
            .default_output_device()
            .ok_or_else(|| anyhow::anyhow!("Failed to open default audio output"))?;

        let audio = self.audio.clone();

        let config = output_device.default_output_config()?.into();
        let stream = output_device.build_output_stream(
            &config,
            move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                audio.play(data, config.channels as usize, config.sample_rate.0 as f64);
            },
            err_fn,
            None,
        )?;

        self.stream = Some(stream);

        Ok(())
    }
}

fn err_fn(err: cpal::StreamError) {
    eprintln!("an error occurred on stream: {}", err);
}
