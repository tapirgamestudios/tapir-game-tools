use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
// 0 = A0, 1 = A#0, 2 = B0, 3 = C0...
pub struct Note(usize);

impl Note {
    pub fn from_white_note(white_note_index: usize) -> Self {
        let octave = white_note_index / 7; // number of white notes in an octave
        let offset = white_note_index % 7;

        static WHITE_NOTE_OFFSETS: [usize; 7] = [0, 2, 3, 5, 7, 8, 10];

        Self(octave * 12 + WHITE_NOTE_OFFSETS[offset])
    }

    pub fn from_sharpened_white_note(white_note_index: usize) -> Option<Self> {
        let octave = white_note_index / 7; // number of white notes in an octave
        let offset = white_note_index % 7;

        static BLACK_NOTE_OFFSETS: [Option<usize>; 7] =
            [Some(1), None, Some(4), Some(6), None, Some(9), Some(11)];

        BLACK_NOTE_OFFSETS[offset].map(|offset| Self(octave * 12 + offset))
    }

    pub fn octave(self) -> usize {
        self.0 / 12
    }

    pub fn note(self) -> usize {
        self.0 % 12
    }

    pub fn from_raw(raw_note: usize) -> Note {
        Self(raw_note)
    }

    pub fn is_black_key(self) -> bool {
        matches!(self.note(), 1 | 4 | 6 | 9 | 11)
    }
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        static NOTES: [&str; 12] = [
            "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#",
        ];

        let octave = self.octave();
        let note = self.note();

        write!(f, "{}{octave}", NOTES[note])
    }
}
