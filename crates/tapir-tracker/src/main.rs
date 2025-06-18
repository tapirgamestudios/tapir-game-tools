#![warn(clippy::all)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

fn main() -> eframe::Result {
    env_logger::init();

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([400., 300.])
            .with_min_inner_size([300., 220.]),
        ..Default::default()
    };

    eframe::run_native(
        "Tapir tracker",
        native_options,
        Box::new(|cc| Ok(Box::new(tapir_tracker::App::new(cc)))),
    )
}
