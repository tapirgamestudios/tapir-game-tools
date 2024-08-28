#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release
#![deny(clippy::all)]

use std::env;

use app::TapirTrackerApp;

mod app;

fn main() -> Result<(), eframe::Error> {
    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions::default();

    let args: Vec<_> = env::args().collect();

    eframe::run_native(
        "Tapir Tracker",
        options,
        Box::new(move |cc| Ok(Box::new(TapirTrackerApp::new(cc, args.get(1).cloned())))),
    )
}
