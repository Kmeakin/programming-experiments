use std::{fmt, ops};

#[derive(Copy, Clone)]
pub struct BitString<const N: usize> {
    /// Stored in BE order on `AArch64`.
    /// Stored in LE order on other architectures.
    /// Conceptually, always stored in BE order, to match the way text is read
    /// from left to right.
    bits: u64,
}

impl<const BITS: usize> BitString<BITS> {
    pub fn from_le_bits(bits: u64) -> Self {
        let bits = if cfg!(target_arch = "aarch64") {
            bits.reverse_bits()
        } else {
            bits
        };
        Self { bits }
    }

    pub fn from_be_bits(bits: u64) -> Self {
        let bits = if cfg!(target_arch = "aarch64") {
            bits
        } else {
            bits.reverse_bits()
        };
        Self { bits }
    }

    pub fn le_bits(self) -> u64 {
        if cfg!(target_arch = "aarch64") {
            self.bits.reverse_bits()
        } else {
            self.bits
        }
    }

    pub fn be_bits(self) -> u64 {
        if cfg!(target_arch = "aarch64") {
            self.bits
        } else {
            self.bits.reverse_bits()
        }
    }

    pub fn new(bits: u64) -> Self { Self::from_le_bits(bits) }

    pub fn leading_zeros(self) -> usize {
        let count = if cfg!(target_arch = "aarch64") {
            self.bits.leading_zeros()
        } else {
            self.bits.trailing_zeros()
        };
        Ord::min(count as usize, BITS)
    }

    pub fn leading_ones(self) -> usize {
        let count = if cfg!(target_arch = "aarch64") {
            self.bits.leading_ones()
        } else {
            self.bits.trailing_ones()
        };
        Ord::min(count as usize, BITS)
    }

    pub fn any(self) -> bool { self.leading_ones() > 0 }
}

impl<const BITS: usize> fmt::Debug for BitString<BITS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bits = self.le_bits();
        match BITS {
            16 => write!(f, "{:0BITS$b}", (bits as u16).reverse_bits()),
            32 => write!(f, "{:0BITS$b}", (bits as u32).reverse_bits()),
            64 => write!(f, "{:0BITS$b}", bits.reverse_bits()),
            _ => unreachable!(),
        }
    }
}

impl<const BITS: usize> ops::Shl<usize> for BitString<BITS> {
    type Output = Self;
    #[track_caller]
    fn shl(self, amount: usize) -> Self::Output {
        debug_assert!(amount <= BITS, "amount = {amount}, BITS = {BITS}");
        let bits = if cfg!(target_arch = "aarch64") {
            self.bits.unbounded_shl(amount as u32)
        } else {
            self.bits.unbounded_shr(amount as u32)
        };
        Self { bits }
    }
}

impl<const BITS: usize> ops::ShlAssign<usize> for BitString<BITS> {
    fn shl_assign(&mut self, amount: usize) { *self = *self << amount; }
}

impl<const BITS: usize> ops::BitOr for BitString<BITS> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            bits: self.bits | rhs.bits,
        }
    }
}

impl<const BITS: usize> ops::BitAnd for BitString<BITS> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            bits: self.bits & rhs.bits,
        }
    }
}

impl<const BITS: usize> ops::BitXor for BitString<BITS> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self {
            bits: self.bits ^ rhs.bits,
        }
    }
}

impl<const BITS: usize> ops::Not for BitString<BITS> {
    type Output = Self;
    fn not(self) -> Self::Output { Self { bits: !self.bits } }
}
