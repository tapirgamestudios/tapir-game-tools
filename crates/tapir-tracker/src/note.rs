use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
// 0 = A0, 1 = A#0, 2 = B0, 3 = C0...
pub struct Note(usize);

impl Note {
    pub fn from_note_number(note_number: usize) -> Note {
        Self(note_number)
    }

    pub fn octave(self) -> usize {
        self.0 / 12
    }

    pub fn note(self) -> usize {
        self.0 % 12
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

impl fmt::Debug for Note {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Note({self})")
    }
}
