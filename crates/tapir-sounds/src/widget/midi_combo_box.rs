use eframe::egui;

use crate::midi;

pub fn midi_combo_box(ui: &mut egui::Ui, midi: &mut midi::Midi) -> Result<(), midi::MidiError> {
    let display_name = midi.selected_device_name();

    let mut selected_device = midi.selected_device();
    egui::ComboBox::from_id_source("midi input combobox")
        .selected_text(display_name)
        .width(200.0)
        .show_ui(ui, |ui| {
            ui.selectable_value(&mut selected_device, None, "No midi input");

            for in_port in midi.devices() {
                let Some(port_name) = midi.device_name(&in_port) else {
                    continue;
                };

                ui.selectable_value(&mut selected_device, Some(in_port), port_name);
            }
        });

    midi.update_selected_device(&selected_device)
}
