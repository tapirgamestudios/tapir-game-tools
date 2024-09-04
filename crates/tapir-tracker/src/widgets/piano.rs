use catppuccin_egui::Theme;
use egui::{
    emath::RectTransform, epaint::RectShape, Align2, Color32, FontId, Pos2, Rect, Rounding, Sense,
    Stroke, Vec2,
};

// pub const NUM_PIANO_KEYS: usize = 88;
pub const NUM_WHITE_KEYS: usize = 52;

const PIANO_KEY_WIDTH: f32 = 64f32;
pub const PIANO_KEY_HEIGHT: f32 = 16f32;

pub fn piano(ui: &mut egui::Ui, theme: &Theme) {
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
        Pos2::new(0., PIANO_KEY_HEIGHT - BLACK_KEY_HEIGHT / 2.),
        Vec2::new(PIANO_KEY_WIDTH / 1.5, BLACK_KEY_HEIGHT),
    );

    for key in 0..NUM_WHITE_KEYS {
        let this_key_rect = to_screen
            .transform_rect(white_key_rect.translate(Vec2::new(0., key as f32 * PIANO_KEY_HEIGHT)));
        painter.add(RectShape::new(
            this_key_rect,
            Rounding::ZERO,
            Color32::WHITE,
            Stroke::new(1., theme.text),
        ));

        let key = NUM_WHITE_KEYS - key - 1;
        let note = key % 7;
        let value = key / 7;

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
    // minus one on the keys since we don't want to draw A-0b
    for key in 0..(NUM_WHITE_KEYS - 1) {
        let note = key % 7;
        // draw the sharp black note
        if note != 0 && note != 4 {
            let this_black_key_rect = to_screen.transform_rect(
                black_key_rect.translate(Vec2::new(0., key as f32 * PIANO_KEY_HEIGHT)),
            );

            painter.add(RectShape::new(
                this_black_key_rect,
                Rounding::ZERO,
                Color32::BLACK,
                Stroke::new(1., theme.base),
            ));
        }
    }
}
