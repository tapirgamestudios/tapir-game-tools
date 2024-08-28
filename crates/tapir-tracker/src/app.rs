pub struct TapirTrackerApp {}

impl TapirTrackerApp {
    pub fn new(cc: &eframe::CreationContext<'_>, _filename: Option<String>) -> Self {
        catppuccin_egui::set_theme(&cc.egui_ctx, catppuccin_egui::FRAPPE);

        Self {}
    }
}

impl eframe::App for TapirTrackerApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.label("Hello, world!");
        });
    }
}
