use std::{fs, path::Path, sync::Arc};

pub fn save(state: &tapir_sounds_state::State, filepath: &Path) -> anyhow::Result<()> {
    let persisted_state = tapir_sounds_state::persistance::PersistedState::new_from_state(state);

    let output = ron::ser::to_string_pretty(&persisted_state, ron::ser::PrettyConfig::default())?;
    fs::write(filepath, output)?;

    Ok(())
}

pub fn load(
    filepath: &Path,
    block_factory: &tapir_sounds_state::BlockFactory,
) -> anyhow::Result<tapir_sounds_state::State> {
    let content = fs::read_to_string(filepath)?;
    let deserialized: tapir_sounds_state::persistance::PersistedState = ron::from_str(&content)?;

    Ok(deserialized.to_state(block_factory))
}

pub fn export(filepath: &Path, data: &[f64], frequency: f64) -> anyhow::Result<()> {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: frequency as u32,
        bits_per_sample: 16,
        sample_format: hound::SampleFormat::Int,
    };

    let mut writer = hound::WavWriter::create(filepath, spec)?;

    for sample in data {
        writer.write_sample((*sample * i16::MAX as f64) as i16)?;
    }

    writer.finalize()?;

    Ok(())
}

pub fn import(filepath: &Path) -> anyhow::Result<Arc<[f64]>> {
    let mut wav_file = hound::WavReader::open(filepath)?;

    let samples = match wav_file.spec().sample_format {
        hound::SampleFormat::Float => wav_file
            .samples::<f32>()
            .map(|sample| Ok(sample? as f64))
            .collect::<Result<_, hound::Error>>()?,
        hound::SampleFormat::Int => {
            let bits_per_sample = wav_file.spec().bits_per_sample as u32;
            let max = 2usize.pow(bits_per_sample - 1) as f64;

            wav_file
                .samples::<i32>()
                .map(|sample| Ok(sample? as f64 / max))
                .collect::<Result<_, hound::Error>>()?
        }
    };

    Ok(samples)
}
