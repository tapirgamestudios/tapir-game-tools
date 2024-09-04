use catppuccin_egui::Theme;
use egui::{
    emath::RectTransform, epaint::RectShape, Align2, Color32, FontId, Pos2, Rect, Rounding, Sense,
    Stroke, Vec2,
};

pub const NUM_PIANO_KEYS: usize = 88;
pub const NUM_WHITE_KEYS: usize = 52;

const PIANO_KEY_WIDTH: f32 = 64f32;
pub const PIANO_KEY_HEIGHT: f32 = 16f32;

#[derive(Clone, Copy, PartialEq, Eq)]
// 0 = A0, 1 = A#0, 2 = B0, 3 = C0...
pub struct Note(usize);

impl Note {
    pub fn from_y(y: f32) -> Self {
        let y = NUM_WHITE_KEYS as f32 * PIANO_KEY_HEIGHT - y;

        let total_piano_height = NUM_WHITE_KEYS as f32 * PIANO_KEY_HEIGHT;
        let single_key_height = total_piano_height / NUM_PIANO_KEYS as f32;

        Self((y / single_key_height) as usize)
    }

    pub fn from_white_note(white_note_index: usize) -> Self {
        let octave = white_note_index / 7; // number of white notes in an octave
        let offset = white_note_index % 7;

        static WHITE_NOTE_OFFSETS: &[usize] = &[0, 2, 3, 5, 7, 8, 10];

        Self(octave * 12 + WHITE_NOTE_OFFSETS[offset])
    }

    pub fn from_sharpened_white_note(white_note_index: usize) -> Option<Self> {
        let octave = white_note_index / 7; // number of white notes in an octave
        let offset = white_note_index % 7;

        static BLACK_NOTE_OFFSETS: &[Option<usize>] =
            &[Some(1), None, Some(4), Some(6), None, Some(9), Some(11)];

        BLACK_NOTE_OFFSETS[offset].map(|offset| Self(octave * 12 + offset))
    }
}

pub fn piano(ui: &mut egui::Ui, theme: &Theme, highlighted_note: Option<Note>) {
    let (response, painter) = ui.allocate_painter(
        Vec2::new(PIANO_KEY_WIDTH, NUM_WHITE_KEYS as f32 * PIANO_KEY_HEIGHT),
        Sense::hover(),
    );

    let to_screen = RectTransform::from_to(
        Rect::from_min_size(Pos2::ZERO, response.rect.size()),
        response.rect,
    );

    const BLACK_KEY_HEIGHT: f32 = PIANO_KEY_HEIGHT * 0.6;
    let white_key_rect =
        Rect::from_min_size(Pos2::ZERO, Vec2::new(PIANO_KEY_WIDTH, PIANO_KEY_HEIGHT));
    let black_key_rect = Rect::from_min_size(
        Pos2::new(0., -BLACK_KEY_HEIGHT / 2.),
        Vec2::new(PIANO_KEY_WIDTH / 1.5, BLACK_KEY_HEIGHT),
    );

    for key in 0..NUM_WHITE_KEYS {
        let this_key_rect = to_screen
            .transform_rect(white_key_rect.translate(Vec2::new(0., key as f32 * PIANO_KEY_HEIGHT)));

        let key = NUM_WHITE_KEYS - key - 1;
        let note = key % 7;
        let value = key / 7;

        let this_note = Note::from_white_note(key);

        let colour = if highlighted_note == Some(this_note) {
            Color32::LIGHT_RED
        } else {
            Color32::WHITE
        };

        painter.add(RectShape::new(
            this_key_rect,
            Rounding::ZERO,
            colour,
            Stroke::new(1., theme.text),
        ));

        static ALPHABET: &[char] = &['A', 'B', 'C', 'D', 'E', 'F', 'G'];

        painter.text(
            this_key_rect.right_center(),
            Align2::RIGHT_CENTER,
            format!("{}{value}", ALPHABET[note]),
            FontId::monospace(12.),
            theme.surface0,
        );
    }

    // Draw the black notes after the white ones to ensure they're on top.
    // minus one on the keys since we don't want to draw C#7
    for key in 1..NUM_WHITE_KEYS {
        let this_black_key_rect = to_screen
            .transform_rect(black_key_rect.translate(Vec2::new(0., key as f32 * PIANO_KEY_HEIGHT)));
        let key = NUM_WHITE_KEYS - key - 1;

        let note = Note::from_sharpened_white_note(key);
        // draw the sharp black note
        if let Some(note) = note {
            let colour = if highlighted_note == Some(note) {
                Color32::LIGHT_RED
            } else {
                Color32::BLACK
            };

            painter.add(RectShape::new(
                this_black_key_rect,
                Rounding::ZERO,
                colour,
                Stroke::new(1., theme.base),
            ));
        }
    }
}
