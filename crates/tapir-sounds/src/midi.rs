use std::sync::Arc;

use crate::audio;

#[derive(Clone, PartialEq)]
pub struct MidiDevice(midir::MidiInputPort);

pub struct Midi {
    audio: Arc<audio::Audio>,

    selected_midi_device: Option<MidiDevice>,
    midi_input_ports: midir::MidiInput,
    midi_connection: Option<midir::MidiInputConnection<()>>,
}

impl Midi {
    pub fn new(audio: Arc<audio::Audio>) -> Result<Self, MidiError> {
        let midi_input_ports = match midir::MidiInput::new("tapir sounds") {
            Ok(mut midi_input_ports) => {
                midi_input_ports.ignore(midir::Ignore::None);
                Ok(midi_input_ports)
            }
            Err(e) => {
                return Err(MidiError::Warning(format!("Failed to initialize midi, you will not be able to use midi input in this session {}", e)));
            }
        }?;

        Ok(Self {
            audio,

            selected_midi_device: None,
            midi_input_ports,
            midi_connection: None,
        })
    }

    pub fn device_name(&self, device: &MidiDevice) -> Option<String> {
        self.midi_input_ports.port_name(&device.0).ok()
    }

    pub fn selected_device(&self) -> Option<MidiDevice> {
        self.selected_midi_device.clone()
    }

    pub fn selected_device_name(&self) -> String {
        let mut name = self
            .selected_midi_device
            .as_ref()
            .and_then(|device| self.midi_input_ports.port_name(&device.0).ok())
            .unwrap_or_else(|| "No midi device".to_owned());
        name.truncate(25);

        name
    }

    pub fn devices(&self) -> impl Iterator<Item = MidiDevice> + '_ {
        self.midi_input_ports.ports().into_iter().map(MidiDevice)
    }

    pub fn update_selected_device(
        &mut self,
        selected_midi_device: &Option<MidiDevice>,
    ) -> Result<(), MidiError> {
        if selected_midi_device == &self.selected_midi_device {
            return Ok(());
        }

        if let Some(in_port) = selected_midi_device {
            let midi_input = match midir::MidiInput::new("Tapir sounds - midi input") {
                Ok(input) => input,
                Err(e) => {
                    return Err(MidiError::Error(format!(
                        "Failed to initialize midi device {}",
                        e
                    )))
                }
            };
            let audio = self.audio.clone();

            fn midi_to_speed(key: u8) -> f64 {
                2.0f64.powf(((key as f64) - 69.0) / 12.0)
            }

            let mut current_note = 0u8;

            self.midi_connection = Some(
                match midi_input.connect(
                    &in_port.0,
                    "tapir-sounds-in",
                    move |_, message, _| {
                        let event = midly::live::LiveEvent::parse(message).unwrap();

                        if let midly::live::LiveEvent::Midi { message, .. } = event {
                            match message {
                                midly::MidiMessage::NoteOn { key, .. } => {
                                    audio.play_at_speed(midi_to_speed(key.into()));
                                    current_note = key.into();
                                }
                                midly::MidiMessage::NoteOff { key, .. } => {
                                    let key: u8 = key.into();
                                    if current_note == key {
                                        audio.stop_playing();
                                    }
                                }
                                _ => {}
                            }
                        }
                    },
                    (),
                ) {
                    Ok(connection) => connection,
                    Err(e) => {
                        return Err(MidiError::Warning(format!(
                            "Failed to create midi connection: {e}"
                        )));
                    }
                },
            );
        } else {
            self.midi_connection = None;
        }

        Ok(())
    }
}

pub enum MidiError {
    Warning(String),
    Error(String),
}
