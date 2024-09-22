use catppuccin_egui::Theme;
use egui::{
    emath::RectTransform, epaint::RectShape, vec2, Align2, Color32, FontId, Pos2, Rect, Rounding,
    Sense, Stroke,
};

use crate::note::Note;

pub const NUM_PIANO_KEYS: usize = 88;

const PIANO_KEY_WIDTH: f32 = 64f32;
pub const PIANO_KEY_HEIGHT: f32 = 18f32;

pub fn note_from_y(y: f32) -> Note {
    let y = NUM_PIANO_KEYS as f32 * PIANO_KEY_HEIGHT - y;
    Note::from_note_number((y / PIANO_KEY_HEIGHT) as usize)
}

pub fn piano(ui: &mut egui::Ui, theme: &Theme, highlighted_note: Option<Note>) {
    let (response, painter) = ui.allocate_painter(
        vec2(PIANO_KEY_WIDTH, NUM_PIANO_KEYS as f32 * PIANO_KEY_HEIGHT),
        Sense::hover(),
    );

    let to_screen = RectTransform::from_to(
        Rect::from_min_size(Pos2::ZERO, response.rect.size()),
        response.rect,
    );

    let white_key_rect = Rect::from_min_size(Pos2::ZERO, vec2(PIANO_KEY_WIDTH, PIANO_KEY_HEIGHT));
    let black_key_rect =
        Rect::from_min_size(Pos2::ZERO, vec2(PIANO_KEY_WIDTH * 0.7, PIANO_KEY_HEIGHT));

    for key in 0..NUM_PIANO_KEYS {
        let note: Note = Note::from_note_number(NUM_PIANO_KEYS - key - 1);

        let this_key_rect = to_screen.transform_rect(
            if note.is_black_key() {
                black_key_rect
            } else {
                white_key_rect
            }
            .translate(vec2(0., key as f32 * PIANO_KEY_HEIGHT)),
        );

        let colour = if highlighted_note == Some(note) {
            Color32::LIGHT_RED
        } else if note.is_black_key() {
            Color32::BLACK
        } else {
            Color32::WHITE
        };

        painter.add(RectShape::new(
            this_key_rect,
            Rounding::ZERO,
            colour,
            Stroke::new(1., theme.text),
        ));

        if !note.is_black_key() {
            painter.text(
                this_key_rect.right_center(),
                Align2::RIGHT_CENTER,
                format!("{note}"),
                FontId::monospace(12.),
                theme.surface0,
            );
        }
    }
}
