//! # Status
//!
//! `status` is a collection of utilities for managing the status register flags of a 6502 CPU.
//!
//! Initialize a mutable status register with `ProcessorStatus::new()`
//!
//! Use the Flag enum as arguments to functions, like `get` and `set`, on the status register.
//!
//! The following flags are available:
//!
//! | Enum    | Bit Representation | Description        |
//! |---------|--------------------|--------------------|
//! | Flag::C | 0b0000_0001        | Carry Flag         |
//! | Flag::Z | 0b0000_0010        | Zero Flag          |
//! | Flag::I | 0b0000_0100        | Interrupt Disable  |  
//! | Flag::D | 0b0000_1000        | Decimal Mode Flag  |   
//! | Flag::B | 0b0001_0000        | Break Command      |   
//! | Flag::V | 0b0100_0000        | Overflow Flag      |
//! | Flag::N | 0b1000_0000        | Negative Flag      |

const STATUS_RESET: u8 = 0b0010_0100;

/// Represents the individual flags of a 6502 status register
#[derive(PartialEq, Debug)]
pub enum Flag {
    /// Carry Flag `0b0000_0001`
    ///
    /// - After ADC, this is the carry result of the addition.
    /// - After SBC or CMP, this flag will be set if no borrow was the result, or alternatively a "greater than or equal" result.
    /// - After a shift instruction (ASL, LSR, ROL, ROR), this contains the bit that was shifted out.
    /// - Increment and decrement instructions do not affect the carry flag.
    /// - Can be set or cleared directly with SEC, CLC.
    C = 0b0000_0001,

    /// Zero Flag `0b0000_0010`
    ///
    /// After most instructions that have a value result, this flag will either be set or cleared based on whether or not that value is equal to zero.
    Z = 0b0000_0010,

    /// Interrupt Disable Flag `0b0000_0100`
    ///
    /// - When set, all interrupts except the NMI are inhibited.
    /// - Can be set or cleared directly with SEI, CLI.
    /// - Automatically set by the CPU when an IRQ is triggered, and restored to its previous state by RTI.
    /// - If the /IRQ line is low (IRQ pending) when this flag is cleared, an interrupt will immediately be triggered.
    I = 0b0000_0100,

    /// Decimal Flag `0b0000_1000`
    ///
    /// - On the NES, this flag has no effect.
    /// - On the original 6502, this flag causes some arithmetic instructions to use binary-coded decimal representation to make base 10 calculations easier.
    /// - Can be set or cleared directly with SED, CLD.
    D = 0b0000_1000,

    /// Break Flag (1) `0b0001_0000`
    ///
    /// Does nothing, but is sometimes set when pushed to the stack.
    B = 0b0001_0000,

    /// Break Flag (2) `0b0010_0000`
    ///
    /// Does nothing, but is sometimes set when pushed to the stack.
    S = 0b0010_0000,

    /// Overflow Flag `0b0100_0000`
    ///
    /// - ADC and SBC will set this flag if the signed result would be invalid, necessary for making signed comparisons.
    /// - BIT will load bit 6 of the addressed value directly into the V flag.
    /// - Can be cleared directly with CLV. There is no corresponding set instruction.
    V = 0b0100_0000,

    /// Negative Flag `0b1000_0000`
    ///
    /// - After most instructions that have a value result, this flag will contain bit 7 of that result.
    /// - BIT will load bit 7 of the addressed value directly into the N flag.
    N = 0b1000_0000,
}

impl Flag {
    /// Get the u8 value of an individual flag.
    ///
    /// # Example
    /// ```
    /// # use nes_emulator::cpu::status::Flag;
    /// let flag = Flag::C;
    /// let value = flag.as_u8();
    ///
    /// assert_eq!(value, 0b0000_0001);
    /// ```
    pub fn as_u8(&self) -> u8 {
        match self {
            Flag::C => 0b0000_0001,
            Flag::Z => 0b0000_0010,
            Flag::I => 0b0000_0100,
            Flag::D => 0b0000_1000,
            Flag::B => 0b0001_0000,
            Flag::S => 0b0010_0000,
            Flag::V => 0b0100_0000,
            Flag::N => 0b1000_0000,
        }
    }

    /// Returns a status Flag enum from a u8 value.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::Flag;
    /// let value = 0b1000_0000;
    /// let flag = Flag::from_u8(value);
    ///
    /// assert_eq!(flag, Flag::N);
    /// ```
    ///
    /// # Panics
    ///
    /// If a u8 value with zero or multiple bits set is provided, the value cannot be converted into a single enum Flag and will panic.
    pub fn from_u8(value: u8) -> Flag {
        match value {
            0b0000_0001 => Flag::C,
            0b0000_0010 => Flag::Z,
            0b0000_0100 => Flag::I,
            0b0000_1000 => Flag::D,
            0b0001_0000 => Flag::B,
            0b0100_0000 => Flag::V,
            0b1000_0000 => Flag::N,
            _ => panic!("Attempted to get single flag enum from invalid value (multiple bits or no bits were set on value {}).", value),
        }
    }
}

/// A struct that represents the status register of a 6502 CPU
pub struct ProcessorStatus {
    current: u8,
}

/// # Initialization
impl ProcessorStatus {
    /// Creates and returns a new status register for a 6502 CPU with all bit flags unset.
    pub fn new() -> Self {
        ProcessorStatus {
            current: STATUS_RESET,
        }
    }

    /// Creates and returns a new status register for a 6502 CPU with flags set to the provided u8 binary value.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let status = ProcessorStatus::from(0b0100_1111);
    /// # assert_eq!(status.value(), 0b0100_1111);
    /// ```
    pub fn from(value: u8) -> Self {
        ProcessorStatus { current: value }
    }

    /// Creates and returns a new status register for a 6502 CPU with the provided flags set.
    /// The flags must be passed in a vector and each flag will be applied to the initial value of the status register.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let status = ProcessorStatus::from_flags(vec![Flag::C, Flag::Z, Flag::N]);
    /// # assert_eq!(status.value(), 0b1000_0011);
    /// ```
    pub fn from_flags(flags: Vec<Flag>) -> Self {
        let mut status = ProcessorStatus::from(0);
        status.set_many(flags);
        status
    }
}

/// # Flag Getters/Setters
impl ProcessorStatus {
    /// Get the boolean value of a provided flag representing whether or not that flag is currently set.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::from_flags(vec![Flag::C, Flag::Z]);
    /// let result = status.get(Flag::Z);
    ///
    /// assert_eq!(result, true);
    /// ```
    pub fn get(&self, flag: Flag) -> bool {
        let flag = flag.as_u8();
        self.current & flag == flag
    }

    /// Sets the provided flag. If the provided flag is already set, this does nothing.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::new();
    /// status.set(Flag::Z);
    ///
    /// assert_eq!(status.get(Flag::Z), true);
    /// ```
    pub fn set(&mut self, flag: Flag) {
        let flag = flag.as_u8();
        self.current = self.current | flag;
    }

    /// Unsets the provided flag. If the provided flag is already unset, this does nothing.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::from_flags(vec![Flag::Z]);
    /// status.unset(Flag::Z);
    ///
    /// assert_eq!(status.get(Flag::Z), false);
    /// ```
    pub fn unset(&mut self, flag: Flag) {
        let flag = flag.as_u8();
        self.current = self.current & !flag;
    }

    /// Sets or unsets the provided flag to the value of the provided bool.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::new();
    ///
    /// status.update(Flag::Z, true); // Sets the Z flag
    /// assert_eq!(status.get(Flag::Z), true);
    ///
    /// status.update(Flag::Z, false); // Unsets the Z flag
    /// assert_eq!(status.get(Flag::Z), false);
    /// ```
    pub fn update(&mut self, flag: Flag, value: bool) {
        if value {
            self.set(flag);
        } else {
            self.unset(flag);
        }
    }

    /// Set the provided flag only if the provided u8 value has the corresponding bit set, and unset it otherwise.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::new();
    ///
    /// status.match_bit(Flag::N, 0b1001_0000);
    /// assert_eq!(status.value(), 0b1000_0000);
    ///
    /// status.match_bit(Flag::N, 0b0100_1111);
    /// assert_eq!(status.value(), 0b0000_0000);
    /// ```
    pub fn match_bit(&mut self, flag: Flag, value: u8) {
        let flag_value = flag.as_u8();
        self.update(flag, flag_value & value == flag_value);
    }

    fn merge_flags(flags: Vec<Flag>) -> u8 {
        flags.iter().fold(0, |acc, flag| acc + flag.as_u8())
    }

    /// Sets multiple flags at once. Pass in a vector of flags and each will be applied. If any are already set, they will remain set.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::new();
    /// status.set_many(vec![Flag::D, Flag::N, Flag::C]);
    ///
    /// assert_eq!(status.value(), 0b1000_1001);
    /// ```
    pub fn set_many(&mut self, flags: Vec<Flag>) {
        self.current = self.current | Self::merge_flags(flags);
    }

    /// Unsets multiple flags at once. Pass in a vector of flags and each will be unset. If any are already unset, they will remain unset.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::from(0b1100_1111);
    /// status.unset_many(vec![Flag::N, Flag::V, Flag::D]);
    ///
    /// assert_eq!(status.value(), 0b0000_0111);
    /// ```
    pub fn unset_many(&mut self, flags: Vec<Flag>) {
        self.current = self.current & !Self::merge_flags(flags);
    }

    /// Updates multiple flags at once to the provided boolean value. Pass in a vector of flags and each will be set or unset based on the provided boolen.
    /// If any flags already match the provided boolean, they will remain in that state.
    ///
    /// # Example
    ///
    /// ```
    /// # use nes_emulator::cpu::status::{Flag, ProcessorStatus};
    /// let mut status = ProcessorStatus::from(0b1000_0001);
    ///
    /// status.update_many(vec![Flag::N, Flag::V, Flag::D], true);
    /// assert_eq!(status.value(), 0b1100_1001);
    ///
    /// status.update_many(vec![Flag::C, Flag::N], false);
    /// assert_eq!(status.value(), 0b0100_1000);
    /// ```
    pub fn update_many(&mut self, flags: Vec<Flag>, value: bool) {
        if value {
            self.set_many(flags);
        } else {
            self.unset_many(flags);
        }
    }

    /// Returns the current u8 value of the status register.
    pub fn value(&self) -> u8 {
        self.current
    }

    /// If the provided flag is set, this will return a value with only that associated bit set. Otherwise it returns 0.
    pub fn as_bit_if_set(&self, flag: Flag) -> u8 {
        self.current & flag.as_u8()
    }

    /// Updates the current u8 value of the status register to the provided u8 value.
    pub fn restore(&mut self, value: u8) {
        self.current = value;
    }

    /// Resets the current u8 value of the status register to 0, unsetting all flags.
    pub fn reset(&mut self) {
        self.restore(STATUS_RESET);
    }
}

#[cfg(test)]
mod status_tests {
    use super::*;

    #[test]
    fn initialize_status() {
        let status = ProcessorStatus::from(0);
        assert_eq!(status.current, 0b0000_0000);
        let status = ProcessorStatus::from(0b1100_0011);
        assert_eq!(status.current, 0b1100_0011);
        let status = ProcessorStatus::from_flags(vec![Flag::C, Flag::Z]);
        assert_eq!(status.current, 0b0000_0011);
    }

    #[test]
    fn get_value() {
        let status = ProcessorStatus::from(0b1100_0000);
        assert_eq!(status.value(), 0b1100_0000);
    }

    #[test]
    fn get_status() {
        let status = ProcessorStatus {
            current: 0b0100_0010, // VZ
        };
        assert_eq!(status.get(Flag::Z), true);
        assert_eq!(status.get(Flag::V), true);
        assert_eq!(status.get(Flag::B), false);
    }

    #[test]
    fn set_status() {
        let mut status = ProcessorStatus::from(0);
        assert_eq!(status.current, 0b0000_0000);
        status.set(Flag::C);
        assert_eq!(status.current, 0b0000_0001);
        status.set(Flag::N);
        assert_eq!(status.current, 0b1000_0001);
    }

    #[test]
    fn match_bit() {
        let mut status = ProcessorStatus::from(0);
        status.match_bit(Flag::V, 0b1100_1100);
        assert_eq!(status.current, 0b0100_0000);
        status.match_bit(Flag::C, 0b0000_0011);
        assert_eq!(status.current, 0b0100_0001);
        status.match_bit(Flag::V, 0b1000_1110);
        assert_eq!(status.current, 0b0000_0001);
    }

    #[test]
    fn set_many() {
        let mut status = ProcessorStatus::from(0);
        status.set_many(vec![Flag::V, Flag::Z, Flag::C]);
        assert_eq!(status.current, 0b0100_0011);
    }

    #[test]
    fn unset_status() {
        let mut status = ProcessorStatus {
            current: 0b0001_1000, // BD
        };
        status.unset(Flag::B);
        assert_eq!(status.current, 0b0000_1000);
        status.unset(Flag::D);
        assert_eq!(status.current, 0b0000_0000);
    }

    #[test]
    fn unset_many() {
        let mut status = ProcessorStatus {
            current: 0b1101_1111,
        };
        status.unset_many(vec![Flag::B, Flag::D, Flag::C]);
        assert_eq!(status.current, 0b1100_0110);
    }

    #[test]
    fn update_status() {
        let mut status = ProcessorStatus {
            current: 0b0001_0000, // B
        };
        status.update(Flag::B, false);
        assert_eq!(status.current, 0b0000_0000);
        status.update(Flag::S, true);
        assert_eq!(status.current, 0b0010_0000);
    }

    #[test]
    fn update_many() {
        let mut status = ProcessorStatus::from(0);
        status.update_many(vec![Flag::C, Flag::Z, Flag::I, Flag::D], true);
        assert_eq!(status.current, 0b0000_1111);
        status.update_many(vec![Flag::C, Flag::I], false);
        assert_eq!(status.current, 0b0000_1010);
    }

    #[test]
    fn restore_reset_updates_all_flags() {
        let mut status = ProcessorStatus {
            current: 0b1101_0001,
        };
        status.restore(0b0000_1111);
        assert_eq!(status.current, 0b0000_1111);
        status.reset();
        assert_eq!(status.current, STATUS_RESET);
    }

    #[test]
    fn flag_convert_to_u8_and_back() {
        let flag = Flag::I; // 0b0000_0100
        let value = flag.as_u8();
        assert_eq!(value, 0b0000_0100);
        let flag = Flag::from_u8(value);
        assert_eq!(flag, Flag::I);
    }

    #[test]
    #[should_panic]
    fn flag_panics_when_converting_invalid_value() {
        Flag::from_u8(0b1101_1011);
    }

    #[test]
    fn as_bit_if_set() {
        let mut status = ProcessorStatus::from(0b1100_1100);
        assert_eq!(status.as_bit_if_set(Flag::V), 0b0100_0000);

        status.restore(0b0011_0011);
        assert_eq!(status.as_bit_if_set(Flag::N), 0b0000_0000);
    }
}
