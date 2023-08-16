use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use eframe::egui;

use crate::audio;
use crate::midi;
use crate::save_load;
use crate::widget;
use crate::widget::open_save;
use tapir_sounds_state::calculate;

pub struct TapirSoundApp {
    state: tapir_sounds_state::State,
    calculator: calculate::Calculator,
    last_updated_audio_id: Option<calculate::CalculationId>,

    block_factory: tapir_sounds_state::BlockFactory,

    toasts: egui_notify::Toasts,

    pan: egui::Vec2,

    audio: Arc<audio::Audio>,
    _audio_device: Option<Box<dyn tinyaudio::BaseAudioOutputDevice>>,

    midi: Option<midi::Midi>,

    open_save: open_save::OpenSave,
}

impl TapirSoundApp {
    pub const MAX_NODE_SIZE: [f32; 2] = [200.0, 200.0];

    pub(crate) fn new(cc: &eframe::CreationContext<'_>, file_path: Option<String>) -> Self {
        catppuccin_egui::set_theme(&cc.egui_ctx, catppuccin_egui::FRAPPE);
        let mut toasts = egui_notify::Toasts::default();

        let audio: Arc<audio::Audio> = Default::default();
        let device = match Self::start_sound(audio.clone()) {
            Err(e) => {
                toasts
                    .error(e.to_string())
                    .set_closable(false)
                    .set_duration(None);
                None
            }
            Ok(device) => Some(device),
        };

        toasts
            .info("Double click a block to activate it. Press space to play.")
            .set_duration(Some(Duration::from_secs(5)));

        let file_path: Option<PathBuf> = file_path.map(|path| path.into());

        let midi = match midi::Midi::new(audio.clone()) {
            Ok(midi) => Some(midi),
            Err(e) => {
                match e {
                    midi::MidiError::Warning(text) => toasts.warning(text),
                    midi::MidiError::Error(text) => toasts.error(text),
                };
                None
            }
        };

        let mut app = Self {
            state: Default::default(),
            calculator: Default::default(),
            block_factory: tapir_sounds_state::BlockFactory::new(),
            pan: Default::default(),
            last_updated_audio_id: None,
            toasts,

            audio,
            _audio_device: device,

            midi,

            open_save: open_save::OpenSave::new(file_path.clone()),
        };

        if let Some(file_path) = file_path {
            app.open(&file_path);
        }

        app
    }

    fn start_sound(
        audio: Arc<audio::Audio>,
    ) -> anyhow::Result<Box<dyn tinyaudio::BaseAudioOutputDevice>> {
        let params = tinyaudio::OutputDeviceParameters {
            channels_count: 2,
            sample_rate: 44100,
            channel_sample_count: 441,
        };

        tinyaudio::run_output_device(params, move |data| {
            audio.play(data, params.channels_count, params.sample_rate as f64);
        })
        .map_err(|e| anyhow::anyhow!("Failed to initialize tinyaudio: {e}"))
    }

    fn update_audio(&mut self) {
        let results = self.calculator.results();
        self.last_updated_audio_id = results.as_ref().map(|results| results.id());

        if let Some(selected) = self
            .state
            .selected_block()
            .and_then(|id| results.and_then(|result| result.for_block(id).cloned()))
        {
            self.audio.set_buffer(selected, self.state.frequency());
        } else {
            self.audio
                .set_buffer(Default::default(), self.state.frequency());
        }
    }

    fn midi_combo_box(&mut self, ui: &mut egui::Ui) {
        let Some(midi) = &mut self.midi else {
            ui.label("Midi failed to initialize");
            return;
        };

        match widget::midi_combo_box(ui, midi) {
            Ok(()) => {}
            Err(midi::MidiError::Warning(text)) => {
                self.toasts.warning(text);
            }
            Err(midi::MidiError::Error(text)) => {
                self.toasts.error(text);
            }
        }
    }
}

impl eframe::App for TapirSoundApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked()
                        || ui.input(|i| i.modifiers.command && i.key_down(egui::Key::N))
                    {
                        self.state = tapir_sounds_state::State::default();
                        self.open_save = open_save::OpenSave::new(None);
                        ui.close_menu();
                    }

                    if ui.button("Open").clicked() {
                        self.open_as();
                        ui.close_menu();
                    }

                    if let Some(path) = self.open_save.file_name() {
                        if ui.button("Save").clicked() {
                            self.save(&path);
                            ui.close_menu();
                        }
                    } else {
                        ui.add_enabled(false, egui::Button::new("Save"));
                    }

                    if ui.button("Save as...").clicked() {
                        self.save_as();
                        ui.close_menu();
                    }

                    if ui.button("Export").clicked() {
                        self.export_as();
                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("Quit").clicked() {
                        frame.close();
                        ui.close_menu();
                    }
                });
            });
        });

        egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
            ui.horizontal(|ui| {
                let mut should_loop = self.state.should_loop();
                if ui.checkbox(&mut should_loop, "Loop").changed() {
                    self.audio.set_should_loop(should_loop);
                    self.state.set_should_loop(should_loop);
                    self.open_save.mark_dirty();
                };

                self.midi_combo_box(ui);

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if self.calculator.is_calculating() {
                        ui.spinner();
                    }
                });
            });
        });

        let pan = self.pan + ctx.available_rect().size() / 2.0;

        egui::SidePanel::left("input_panel")
            .resizable(false)
            .show(ctx, |ui| {
                ui.heading("Blocks");

                ui.separator();

                ui.with_layout(egui::Layout::top_down(egui::Align::LEFT), |ui| {
                    for block_type in self.block_factory.available_blocks() {
                        if ui.button(&block_type.name).clicked() {
                            let block_pos = ui.clip_rect().center() - pan;
                            self.state.add_block(
                                self.block_factory
                                    .make_block(block_type, (block_pos.x, block_pos.y)),
                            );
                        }
                    }
                });
            });

        let mut selected_changed = false;

        egui::CentralPanel::default().show(ctx, |ui| {
            // need to allocate this first so it gets lowest priority
            let background_response =
                ui.allocate_rect(ui.min_rect(), egui::Sense::click_and_drag());

            let results = self.calculator.results();

            let selected_block = self.state.selected_block();

            let responses = self
                .state
                .blocks()
                .map(|block| {
                    let block_pos = block.pos();
                    let mut child_ui = ui.child_ui_with_id_source(
                        egui::Rect::from_min_size(
                            egui::pos2(block_pos.0, block_pos.1) + pan,
                            Self::MAX_NODE_SIZE.into(),
                        ),
                        egui::Layout::default(),
                        block.id(),
                    );

                    child_ui.set_clip_rect(ui.max_rect());

                    let block_response = widget::block(
                        &mut child_ui,
                        block,
                        selected_block == Some(block.id()),
                        results
                            .as_ref()
                            .and_then(|result| result.for_block(block.id())),
                    );

                    (block.id(), block_response)
                })
                .collect::<HashMap<_, _>>();

            for (id, response) in responses.iter() {
                if !response.alter_input.is_empty() {
                    let block = self.state.get_block_mut(*id).unwrap();
                    for (alteration_index, alteration_value) in &response.alter_input {
                        block.set_input(*alteration_index, alteration_value);
                    }

                    self.open_save.mark_dirty();
                }

                if response.selected {
                    self.state.set_selected_block(*id);
                    selected_changed = true;

                    self.open_save.mark_dirty();
                }

                if response.drag_delta.length_sq() > 0.0 {
                    let block = self.state.get_block_mut(*id).unwrap();
                    block.pos_delta((response.drag_delta.x, response.drag_delta.y));

                    self.open_save.mark_dirty();
                }

                if response.delete {
                    self.state.remove_block(*id);
                    self.open_save.mark_dirty();
                }
            }

            let mut cable_ui = ui.child_ui(ui.max_rect(), *ui.layout());
            cable_ui.set_clip_rect(ui.min_rect());
            let cable_response = tapir_cables::cables(
                &mut cable_ui,
                self.state
                    .connections()
                    .map(|(output_block_id, (input_block_id, index))| {
                        (
                            tapir_cables::PortId::new(
                                output_block_id,
                                0,
                                tapir_cables::PortDirection::Output,
                            ),
                            tapir_cables::PortId::new(
                                input_block_id,
                                index,
                                tapir_cables::PortDirection::Input,
                            ),
                        )
                    }),
                catppuccin_egui::FRAPPE.blue,
            );
            if let Some((output, input)) = cable_response.new_connection {
                self.state
                    .add_connection((output.block_id, (input.block_id, input.index)));

                self.open_save.mark_dirty();
            }

            if background_response.dragged() && ui.ctx().input(|i| i.pointer.middle_down()) {
                self.pan += ui.ctx().input(|i| i.pointer.delta());
            }
        });

        if selected_changed {
            self.update_audio();
        }

        if self.state.is_dirty() && self.calculator.calculate(&self.state) {
            self.state.clean();
        }

        if ctx.input(|i| i.key_pressed(egui::Key::Space)) {
            self.audio.toggle_playing();
        }

        if ctx.input(|i| i.modifiers.command && i.key_pressed(egui::Key::S)) {
            if let Some(path) = self.open_save.file_name() {
                self.save(&path);
            } else {
                self.save_as();
            }
        }

        if ctx.input(|i| i.modifiers.command && i.key_pressed(egui::Key::E)) {
            self.export_as();
        }

        if ctx.input(|i| i.modifiers.command && i.key_pressed(egui::Key::O)) {
            self.open_as();
        }

        let results = self.calculator.results();
        if results.map(|result| result.id()) != self.last_updated_audio_id {
            self.update_audio();
        }

        if let Some(title_display) = self.open_save.title_display() {
            frame.set_window_title(&("Tapir Sounds - ".to_owned() + &title_display));
        } else {
            frame.set_window_title("Tapir Sounds");
        }

        match self.open_save.show(ctx) {
            open_save::OpenSaveResponse::Nothing => {}
            open_save::OpenSaveResponse::Open(path) => self.open(&path),
            open_save::OpenSaveResponse::Save(path) => self.save(&path),
            open_save::OpenSaveResponse::Export(path) => self.export(&path),
        }

        self.toasts.show(ctx);
    }
}

impl TapirSoundApp {
    fn save_as(&mut self) {
        self.open_save.save_as();
    }

    fn save(&mut self, file_path: &Path) {
        if let Err(e) = save_load::save(&self.state, file_path) {
            self.toasts.error(e.to_string()).set_closable(true);
            return;
        }

        self.open_save.mark_clean();

        self.toasts.basic(format!(
            "Saved to {}",
            file_path.file_name().unwrap().to_string_lossy()
        ));
    }

    fn open(&mut self, filepath: &Path) {
        self.state = match save_load::load(filepath, &self.block_factory) {
            Ok(state) => state,
            Err(e) => {
                self.toasts.error(e.to_string()).set_closable(true);
                return;
            }
        };
        let average_location = self.state.average_location();
        self.pan = -egui::vec2(average_location.0, average_location.1);
        self.audio.set_should_loop(self.state.should_loop());
        self.open_save.mark_clean();
    }

    fn open_as(&mut self) {
        self.open_save.open_as();
    }

    fn export_as(&mut self) {
        if self.calculator.is_calculating() {
            return;
        };

        self.open_save.export_as();
    }

    fn export(&mut self, filepath: &Path) {
        if !self.state.blocks().any(|_| true) {
            self.toasts.warning("Nothing to export");
            return;
        }

        let Some(results) = self.calculator.results() else {
            self.toasts.warning("Did not export as still calculating");
            return;
        };

        let Some(data) = self
            .state
            .selected_block()
            .and_then(|id| results.for_block(id))
        else {
            self.toasts.warning("Did not export as nothing is selected");
            return;
        };

        match save_load::export(filepath, data, self.state.frequency()) {
            Ok(()) => {
                self.toasts.basic(format!(
                    "Exported to {}",
                    filepath.file_name().unwrap().to_string_lossy()
                ));
            }
            Err(e) => {
                self.toasts.error(e.to_string()).set_closable(true);
            }
        };
    }
}
