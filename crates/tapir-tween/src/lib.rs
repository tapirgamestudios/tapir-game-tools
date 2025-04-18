#![no_std]

use core::ops::{Add, Mul};

use agb_fixnum::{Num, num};

pub trait EaseTime {
    fn time(&self) -> usize;
}

pub struct ConstTime<const N: usize>;

impl<const N: usize> EaseTime for ConstTime<N> {
    fn time(&self) -> usize {
        N
    }
}

impl EaseTime for usize {
    fn time(&self) -> usize {
        *self
    }
}

pub struct Tween<K: EaseKind, T: EaseTime, V> {
    start: V,
    end: V,
    frame: usize,
    time: T,
    ease: K,
}

pub trait EaseKind {
    fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N>;
}

pub enum Ease {
    Linear,
    OutQuad,
    OutCube,
    OutQuart,
    InQuad,
    InCube,
    InQuart,
    InOutQuad,
}

impl EaseKind for Ease {
    fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
        match self {
            Ease::Linear => ease::Linear.amount(frame),
            Ease::OutQuad => ease::OutQuad.amount(frame),
            Ease::OutCube => ease::OutCube.amount(frame),
            Ease::OutQuart => ease::OutQuart.amount(frame),
            Ease::InQuad => ease::InQuad.amount(frame),
            Ease::InCube => ease::InCube.amount(frame),
            Ease::InQuart => ease::InQuart.amount(frame),
            Ease::InOutQuad => ease::InOutQuad.amount(frame),
        }
    }
}

pub mod ease {
    use agb_fixnum::{Num, num};

    use crate::EaseKind;

    pub struct Linear;

    impl EaseKind for Linear {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            frame
        }
    }

    pub struct OutQuad;

    impl EaseKind for OutQuad {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            let one_minus = num!(1.) - frame;
            num!(1.) - one_minus * one_minus
        }
    }

    pub struct OutCube;

    impl EaseKind for OutCube {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            let one_minus = num!(1.) - frame;
            num!(1.) - one_minus * one_minus * one_minus
        }
    }

    pub struct OutQuart;

    impl EaseKind for OutQuart {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            let one_minus = num!(1.) - frame;
            let one_minus_sq = one_minus * one_minus;
            num!(1.) - one_minus_sq * one_minus_sq
        }
    }

    pub struct InQuad;

    impl EaseKind for InQuad {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            frame * frame
        }
    }

    pub struct InCube;

    impl EaseKind for InCube {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            frame * frame * frame
        }
    }

    pub struct InQuart;

    impl EaseKind for InQuart {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            let sq = frame * frame;
            sq * sq
        }
    }

    pub struct InOutQuad;

    impl EaseKind for InOutQuad {
        fn amount<const N: usize>(&self, frame: Num<i32, N>) -> Num<i32, N> {
            if frame < num!(0.5) {
                frame * frame * 2
            } else {
                let one_minus = num!(2.) - frame * 2;
                num!(1.) - one_minus * one_minus / 2
            }
        }
    }
}

impl<K: EaseKind, T: EaseTime, V> Tween<K, T, V> {
    pub const fn new(ease: K, time: T, start: V, end: V) -> Self {
        Self {
            start,
            end,
            frame: 0,
            time,
            ease,
        }
    }
}

impl<K: EaseKind, T: EaseTime, V> Tween<K, T, V>
where
    V: Mul<Num<i32, 8>, Output = V> + Add<Output = V> + Copy,
{
    pub fn update(&mut self) {
        if self.frame < self.time.time() {
            self.frame += 1;
        }
    }

    pub fn get(&self) -> V {
        let amount = self
            .ease
            .amount(Num::new(self.frame as i32) / (self.time.time() as i32));

        self.end * amount + self.start * (num!(1.0) - amount)
    }

    pub fn set_end_position(&mut self, position: V) {
        self.start = self.get();
        self.end = position;
        self.frame = 0;
    }

    pub fn set_tween_kind(&mut self, kind: K) {
        self.ease = kind;
    }

    pub fn set_tween_time(&mut self, time: T) {
        self.time = time;
        self.frame = self.frame.min(self.time.time());
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn check_tween_of_number() {
        let mut tween = Tween::new(ease::Linear, ConstTime::<16>, num!(0.), num!(16.));

        assert_eq!(tween.get(), num!(0.));
        tween.update();
        assert_eq!(tween.get(), num!(1.));

        for _ in 0..100 {
            tween.update();
        }

        assert_eq!(tween.get(), num!(16.));
    }
}
