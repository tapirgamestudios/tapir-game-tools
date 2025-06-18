// Implementing based on gbatek and some DMG emulators
// - https://github.com/mohanson/gameboy/blob/master/src/apu.rs
// - https://github.com/mvdnes/rboy/blob/main/src/sound.rs

use blip_buf::BlipBuf;

use bilge::prelude::*;

// Clock is outputed 1 cycle every N cycles.
pub struct Clock {
    pub period: u32,
    pub n: u32,
}

impl Clock {
    pub fn new(period: u32) -> Self {
        Self { period, n: 0x00 }
    }

    pub fn next(&mut self, cycles: u32) -> u32 {
        self.n += cycles;
        let rs = self.n / self.period;
        self.n = self.n % self.period;
        rs
    }
}

// GBA Sound Channel 1 - Tone & Sweep

// Channel 1 Sweep register
#[bitsize(16)]
#[derive(Clone, Copy, PartialEq, Eq, DebugBits, Default)]
struct SquareSweep {
    shift: u3,
    sweep_direction: SweepDirection,
    time: u3,
    _padding: u9,
}

#[bitsize(1)]
#[derive(FromBits, Clone, Copy, PartialEq, Eq, Debug, Default)]
enum SweepDirection {
    #[default]
    Increasing,
    Decreasing,
}

// Channel 1 Duty/Len/Envelope
#[bitsize(16)]
#[derive(Clone, Copy, PartialEq, Eq, DebugBits, Default)]
struct SquareDutyLenEnvelope {
    length: u6,
    duty: WaveDuty,
    envelope_step_time: u3,
    envelope_direction: EnvelopeDirection,
    initial_envelope_volume: u4,
}

#[bitsize(2)]
#[derive(FromBits, Clone, Copy, PartialEq, Eq, Debug, Default)]
enum WaveDuty {
    #[default]
    Eighth,
    Quarter,
    Half,
    ThreeQuarters,
}

#[bitsize(1)]
#[derive(FromBits, Clone, Copy, PartialEq, Eq, Debug, Default)]
enum EnvelopeDirection {
    #[default]
    Decreasing,
    Increasing,
}

// Channel 1 Frequency/Control
#[bitsize(16)]
#[derive(Clone, Copy, PartialEq, Eq, DebugBits, Default)]
struct SquareFrequencyControl {
    frequency: u11,
    _padding: u3,
    should_stop: bool,
    running: bool,
}

struct Channel1 {
    sweep: SquareSweep,
    duty_len_envelope: SquareDutyLenEnvelope,
    frequency: SquareFrequencyControl,
}

struct Channel2 {
    duty_len_envelope: SquareDutyLenEnvelope,
    frequency: SquareFrequencyControl,
}

trait SquareChannel: Default {
    fn sweep(&self) -> SquareSweep;
    fn duty_len_envelope(&self) -> SquareDutyLenEnvelope;
    fn frequency(&self) -> SquareFrequencyControl;
}

impl SquareChannel for Channel1 {
    fn sweep(&self) -> SquareSweep {
        self.sweep
    }

    fn duty_len_envelope(&self) -> SquareDutyLenEnvelope {
        self.duty_len_envelope
    }

    fn frequency(&self) -> SquareFrequencyControl {
        self.frequency
    }
}

impl SquareChannel for Channel2 {
    fn sweep(&self) -> SquareSweep {
        SquareSweep::default()
    }

    fn duty_len_envelope(&self) -> SquareDutyLenEnvelope {
        self.duty_len_envelope
    }

    fn frequency(&self) -> SquareFrequencyControl {
        self.frequency
    }
}

struct SquareChannelPlayer<C: SquareChannel> {
    channel: C,

    volume: u8,
    index: u32,

    clock: Clock,
    blip: Blip,
}

impl SquareChannelPlayer<C: SquareChannel> {
    pub fn new(buffer: BlipBuf) -> Self {
        Self {
            channel: C::default(),

            volume: 0,
            index: 0,

            clock: Clock::new(8192),
            blip: Blip::new(buffer),
        }
    }

    pub fn set(&mut self, channel: C) {}

    pub fn next(&mut self, cycles: u32) {
        let duty_len_envelope = self.channel.duty_len_envelope();
        let pat = match duty_len_envelope.duty() {
            WaveDuty::Eighth => 0b0000_0001,
            WaveDuty::Quarter => 0b000_0011,
            WaveDuty::Half => 0b000_1111,
            WaveDuty::ThreeQuarters => 0b0011_1111,
        };

        let volume = i32::from(duty_len_envelope.initial_envelope_volume());

        let frequency = self.channel.frequency();

        for _ in 0..self.clock.next(cycles) {
            let amplitude =
                if !frequency.running() || duty_len_envelope.initial_envelope_volume() == 0 {
                    0
                } else if (pat >> self.idx) & 1 != 1 {
                    volume
                } else {
                    -volume
                };

            self.blip
                .set(self.blip.from.wrapping_add(self.timer.period), amplitude);
            self.index = (self.index + 1) % 8;
        }
    }
}

struct Blip {
    data: BlipBuf,
    from: u32,
    amplitude: i32,
}

impl Blip {
    fn new(data: BlipBuf) -> Self {
        Self {
            data,
            from: 0,
            amplitude: 0,
        }
    }

    fn set(&mut self, time: u32, amplitude: i32) {
        self.from = time;
        let d = amplitude - self.amplitude;
        self.amplitude = amplitude;
        self.data.add_delta(time, d);
    }
}
