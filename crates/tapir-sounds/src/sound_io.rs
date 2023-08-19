use std::sync::Arc;

use crate::audio;

pub struct SoundIo {
    audio: Arc<audio::Audio>,
    device: Option<Box<dyn tinyaudio::BaseAudioOutputDevice>>,
}

impl SoundIo {
    pub fn new(audio: Arc<audio::Audio>) -> Self {
        Self {
            audio,
            device: None,
        }
    }

    pub fn start_sound(&mut self) -> anyhow::Result<()> {
        let params = tinyaudio::OutputDeviceParameters {
            channels_count: 2,
            sample_rate: 44100,
            channel_sample_count: 441,
        };

        let audio = self.audio.clone();

        let output_device = tinyaudio::run_output_device(params, move |data| {
            audio.play(data, params.channels_count, params.sample_rate as f64);
        })
        .map_err(|e| anyhow::anyhow!("Failed to initialize audio: {e}"))?;

        self.device = Some(output_device);

        Ok(())
    }
}
