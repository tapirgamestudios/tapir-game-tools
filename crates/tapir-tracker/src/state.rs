use crate::note::Note;

#[derive(Debug, Default, Clone)]
pub struct State {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Default, Clone)]
pub struct Pattern {
    pub channels: Vec<PatternChannel>,
}

#[derive(Debug, Default, Clone)]
pub struct PatternChannel {
    pub notes: Vec<PatternNote>,
}

#[derive(Debug, Clone)]
pub struct PatternNote {
    pub start: usize,
    pub end: usize,
    pub note: Note,
}
