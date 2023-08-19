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

        let supported_config = output_device.default_output_config()?;

        let stream = run(&output_device, &supported_config.into(), audio)?;

        self.stream = Some(stream);

        Ok(())
    }
}

fn run(
    output_device: &cpal::Device,
    config: &cpal::StreamConfig,
    audio: Arc<audio::Audio>,
) -> anyhow::Result<cpal::Stream> {
    let channel_count = config.channels as usize;
    let frequency = config.sample_rate.0 as f64;

    let stream = output_device.build_output_stream(
        config,
        move |data, _| {
            audio.play(data, channel_count, frequency);
        },
        err_fn,
        None,
    )?;

    Ok(stream)
}

fn err_fn(err: cpal::StreamError) {
    eprintln!("an error occurred on stream: {}", err);
}
