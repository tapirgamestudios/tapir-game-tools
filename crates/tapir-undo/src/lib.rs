use std::{collections::VecDeque, time::Duration};

pub struct Undoer<State> {
    undos: VecDeque<State>,
    pointer: usize,
    time_between_states: Duration,
    last_insert: f64,
}

impl<State> Undoer<State> {
    pub fn new(time_between_states: Duration) -> Self {
        Self {
            undos: Default::default(),
            pointer: 0,
            time_between_states,
            last_insert: 0.0,
        }
    }

    pub fn has_undo(&self) -> bool {
        self.pointer != 0
    }

    pub fn has_redo(&self) -> bool {
        if self.undos.is_empty() {
            return false;
        }

        self.pointer != self.undos.len() - 1
    }

    pub fn add_undo(&mut self, current_state: State) {
        self.undos.truncate(self.pointer + 1);
        self.pointer = self.undos.len();
        self.undos.push_back(current_state);
    }

    pub fn undo(&mut self) -> Option<&State> {
        if self.pointer == 0 {
            return None;
        }

        self.pointer = self.pointer.saturating_sub(1);
        self.undos.get(self.pointer)
    }

    pub fn redo(&mut self) -> Option<&State> {
        if self.has_redo() {
            self.pointer += 1;
            self.undos.get(self.pointer)
        } else {
            None
        }
    }
}

impl<State: Clone + PartialEq> Undoer<State> {
    // current_time is in seconds
    pub fn feed_state(&mut self, current_time: f64, current_state: &State) {
        match self.undos.get(self.pointer) {
            None => {
                // always put something here
                self.add_undo(current_state.clone());
            }

            Some(latest_undo) => {
                if latest_undo.eq(current_state) {
                    self.last_insert = current_time;
                }

                if current_time - self.last_insert > self.time_between_states.as_secs_f64() {
                    self.add_undo(current_state.clone());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::Undoer;

    #[test]
    fn should_undo_and_redo() {
        let mut undoer = Undoer::new(Duration::from_secs_f64(5.0));

        undoer.add_undo(5);
        undoer.add_undo(6);
        undoer.add_undo(9);

        assert!(undoer.has_undo());
        assert!(!undoer.has_redo());

        assert_eq!(undoer.redo(), None);
        assert_eq!(undoer.undo(), Some(&6));
        assert!(undoer.has_redo());
        assert!(undoer.has_undo());

        assert_eq!(undoer.redo(), Some(&9));

        assert_eq!(undoer.undo(), Some(&6));
        assert_eq!(undoer.undo(), Some(&5));
        assert_eq!(undoer.undo(), None);

        assert_eq!(undoer.redo(), Some(&6));

        undoer.add_undo(12);

        assert!(!undoer.has_redo());
        assert_eq!(undoer.undo(), Some(&6));
    }
}
