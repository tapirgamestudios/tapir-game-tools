#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![deny(clippy::all)]

use std::env;

use app::TapirTrackerApp;
use egui::ViewportBuilder;

mod app;
mod note;
mod widgets;

fn main() -> Result<(), eframe::Error> {
    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).

    let options = eframe::NativeOptions {
        viewport: ViewportBuilder::default()
            .with_maximized(true)
            .with_min_inner_size((1200f32, 800f32))
            .with_title("Tapir Tracker"),
        ..Default::default()
    };

    let args: Vec<_> = env::args().collect();

    eframe::run_native(
        "tapir_tracker",
        options,
        Box::new(move |cc| Ok(Box::new(TapirTrackerApp::new(cc, args.get(1).cloned())))),
    )
}
