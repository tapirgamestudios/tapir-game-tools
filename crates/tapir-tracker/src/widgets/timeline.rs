use catppuccin_egui::Theme;

use egui::{
    emath::RectTransform, epaint::RectShape, Pos2, Rect, Rounding, Sense, Stroke, Ui, Vec2,
};

use super::piano;

const TIMELINE_ITEM_WIDTH: f32 = piano::PIANO_KEY_HEIGHT;

const TIMELINE_TOTAL_HEIGHT: f32 = piano::PIANO_KEY_HEIGHT * piano::NUM_WHITE_KEYS as f32;

pub struct TimelineSettings {
    pub beats_per_bar: usize,
}

pub fn timeline(ui: &mut Ui, theme: &Theme, settings: TimelineSettings) {
    let num_timeline_items = 0x40;

    let (response, painter) = ui.allocate_painter(
        Vec2::new(
            TIMELINE_ITEM_WIDTH * num_timeline_items as f32,
            TIMELINE_TOTAL_HEIGHT,
        ),
        Sense::hover(),
    );

    let to_screen = RectTransform::from_to(
        Rect::from_min_size(Pos2::ZERO, response.rect.size()),
        response.rect,
    );

    let timeline_rect = Rect::from_min_size(
        Pos2::ZERO,
        Vec2::new(TIMELINE_ITEM_WIDTH, TIMELINE_TOTAL_HEIGHT),
    );

    // render the vertical bars
    for i in 0..num_timeline_items {
        let this_rect = to_screen
            .transform_rect(timeline_rect.translate(Vec2::new(i as f32 * TIMELINE_ITEM_WIDTH, 0.)));

        let fill_colour = if i % settings.beats_per_bar == 0 {
            theme.surface0
        } else {
            theme.surface1
        };

        painter.add(RectShape::new(
            this_rect,
            Rounding::ZERO,
            fill_colour,
            Stroke::new(1., theme.base),
        ));
    }
}
