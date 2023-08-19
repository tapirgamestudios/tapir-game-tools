use std::path::PathBuf;

use eframe::egui;

#[derive(Clone, Copy)]
enum SaveState {
    Open,
    SaveAs,
    Export,
    Import,
}

pub struct OpenSave {
    open_file_dialog: Option<(egui_file::FileDialog, SaveState)>,
    file_path: Option<PathBuf>,
    file_dirty: bool,
}

impl OpenSave {
    pub fn new(file_path: Option<PathBuf>) -> Self {
        Self {
            open_file_dialog: None,
            file_path,
            file_dirty: false,
        }
    }

    pub fn mark_dirty(&mut self) {
        self.file_dirty = true;
    }

    pub fn mark_clean(&mut self) {
        self.file_dirty = false;
    }

    pub fn title_display(&self) -> Option<String> {
        let path = self.file_path.as_ref()?;

        Some(path.to_string_lossy().to_string() + if self.file_dirty { "*" } else { "" })
    }

    pub fn file_name(&self) -> Option<PathBuf> {
        self.file_path.clone()
    }

    pub fn export_as(&mut self) {
        let filepath = self.file_path.as_ref().map(|fp| fp.with_extension("wav"));
        self.open_file_dialog
            .get_or_insert_with(|| (Self::save_dialog(filepath, "Export"), SaveState::Export));
    }

    pub fn open_as(&mut self) {
        let file_path = self.file_path.clone();
        self.open_file_dialog
            .get_or_insert_with(|| (Self::open_dialog(file_path, "Open"), SaveState::Open));
    }

    pub fn save_as(&mut self) {
        let file_path = self.file_path.clone();

        self.open_file_dialog
            .get_or_insert_with(|| (Self::save_dialog(file_path, "Save As"), SaveState::SaveAs));
    }

    pub fn import_as(&mut self) {
        self.open_file_dialog
            .get_or_insert_with(|| (Self::open_dialog(None, "Import WAV"), SaveState::Import));
    }

    fn open_dialog(path: Option<PathBuf>, title: &str) -> egui_file::FileDialog {
        let mut dialog = egui_file::FileDialog::open_file(
            path.clone()
                .and_then(|path| path.parent().map(|parent| parent.to_owned())),
        )
        .title(title);

        if let Some(path) = path {
            if let Some(filename) = path.file_name() {
                dialog = dialog.default_filename(filename.to_string_lossy());
            }
        }
        dialog.open();

        dialog
    }

    fn save_dialog(path: Option<PathBuf>, title: &str) -> egui_file::FileDialog {
        let mut dialog = egui_file::FileDialog::save_file(
            path.clone()
                .and_then(|path| path.parent().map(|parent| parent.to_owned())),
        )
        .title(title);

        if let Some(path) = path {
            if let Some(filename) = path.file_name() {
                dialog = dialog.default_filename(filename.to_string_lossy());
            }
        }

        dialog.open();

        dialog
    }
}

impl OpenSave {
    #[must_use]
    pub fn show(&mut self, ctx: &egui::Context) -> OpenSaveResponse {
        let save_action = if let Some((dialog, save_state)) = &mut self.open_file_dialog {
            dialog.show(ctx);

            if dialog.selected() {
                Some((dialog.path().map(|path| path.to_owned()), *save_state))
            } else if matches!(
                dialog.state(),
                egui_file::State::Closed | egui_file::State::Cancelled
            ) {
                Some((None, *save_state))
            } else {
                None
            }
        } else {
            None
        };

        // Need to do this to make the borrow checker happy
        match save_action {
            Some((Some(path), save_state)) => {
                self.open_file_dialog = None;

                match save_state {
                    SaveState::Open => {
                        self.file_path = Some(path.clone());
                        OpenSaveResponse::Open(path)
                    }
                    SaveState::SaveAs => {
                        let new_file_path = path.with_extension("tapir_sound");
                        self.file_path = Some(new_file_path.clone());
                        OpenSaveResponse::Save(new_file_path)
                    }
                    SaveState::Export => OpenSaveResponse::Export(path.with_extension("wav")),
                    SaveState::Import => OpenSaveResponse::Import(path),
                }
            }
            Some((None, _)) => {
                self.open_file_dialog = None;
                OpenSaveResponse::Nothing
            }
            None => OpenSaveResponse::Nothing,
        }
    }
}

pub enum OpenSaveResponse {
    Nothing,
    Open(PathBuf),
    Save(PathBuf),
    Export(PathBuf),
    Import(PathBuf),
}
