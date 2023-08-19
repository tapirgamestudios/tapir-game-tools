use eframe::egui;

fn droppable_input(
    ui: &mut egui::Ui,
    block_id: tapir_sounds_state::Id,
    index: usize,
    f: impl FnOnce(&mut egui::Ui) -> Option<tapir_sounds_state::Input>,
) -> Option<tapir_sounds_state::Input> {
    ui.horizontal(|ui| {
        tapir_cables::port(ui, block_id, index, tapir_cables::PortDirection::Input);

        f(ui)
    })
    .inner
}

fn drop_point_gap(ui: &mut egui::Ui) {
    ui.add_space(ui.spacing().interact_size.x + ui.spacing().item_spacing.x);
}

pub struct InputResponse {
    pub recording_target: Option<usize>,
    pub input_alteration: Option<tapir_sounds_state::Input>,
}

pub fn input(
    ui: &mut egui::Ui,
    name: &str,
    input: &tapir_sounds_state::Input,
    block_id: tapir_sounds_state::Id,
    index: usize,
) -> InputResponse {
    let input = match input {
        tapir_sounds_state::Input::Toggle(toggled) => {
            drop_point_gap(ui);
            let mut toggled = *toggled;

            if ui.checkbox(&mut toggled, name).changed() {
                Some(tapir_sounds_state::Input::Toggle(toggled))
            } else {
                None
            }
        }
        tapir_sounds_state::Input::Frequency(frequency) => {
            let mut frequency = *frequency;

            droppable_input(ui, block_id, index, |ui| {
                ui.label(name);

                if ui
                    .add(
                        egui::DragValue::new(&mut frequency)
                            .clamp_range(0..=10000)
                            .suffix("Hz"),
                    )
                    .changed()
                {
                    return Some(tapir_sounds_state::Input::Frequency(frequency));
                }

                None
            })
        }
        tapir_sounds_state::Input::Amplitude(amplitude) => {
            let mut amplitude = *amplitude;

            droppable_input(ui, block_id, index, |ui| {
                ui.label(name);
                if ui
                    .add(
                        egui::DragValue::new(&mut amplitude)
                            .clamp_range(-1..=1)
                            .speed(0.005),
                    )
                    .changed()
                {
                    return Some(tapir_sounds_state::Input::Amplitude(amplitude));
                }

                None
            })
        }
        tapir_sounds_state::Input::Periods(periods) => {
            let mut periods = *periods;

            ui.horizontal(|ui| {
                drop_point_gap(ui);
                ui.label(name);

                if ui
                    .add(
                        egui::DragValue::new(&mut periods)
                            .clamp_range(0..=1000)
                            .speed(0.025)
                            .max_decimals(1),
                    )
                    .changed()
                {
                    return Some(tapir_sounds_state::Input::Periods(periods));
                }

                None
            })
            .inner
        }
        tapir_sounds_state::Input::Input => droppable_input(ui, block_id, index, |ui| {
            ui.label(name);
            None
        }),
        tapir_sounds_state::Input::Recording(_) => {
            if let Some(input_response) = ui
                .horizontal(|ui| {
                    drop_point_gap(ui);
                    if ui.button("Browse...").clicked() {
                        Some(InputResponse {
                            recording_target: Some(index),
                            input_alteration: None,
                        })
                    } else {
                        None
                    }
                })
                .inner
            {
                return input_response;
            } else {
                None
            }
        }
    };

    InputResponse {
        recording_target: None,
        input_alteration: input,
    }
}
