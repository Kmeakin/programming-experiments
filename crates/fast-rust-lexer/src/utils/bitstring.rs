use std::{fmt, ops};

#[derive(Copy, Clone)]
pub struct BitString<const N: usize> {
    bits: u64,
}

impl<const BITS: usize> BitString<BITS> {
    pub fn new(bits: u64) -> Self { Self { bits } }
    pub fn leading_zeros(self) -> usize { Ord::min(self.bits.leading_zeros() as usize, BITS) }
    pub fn leading_ones(self) -> usize { Ord::min(self.bits.leading_ones() as usize, BITS) }
    pub fn any(self) -> bool { self.leading_ones() > 0 }
}

impl<const BITS: usize> fmt::Debug for BitString<BITS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match BITS {
            16 => write!(f, "{:0BITS$b}", (self.bits >> 48) as u16),
            32 => write!(f, "{:0BITS$b}", (self.bits >> 32) as u32),
            64 => write!(f, "{:0BITS$b}", self.bits),
            _ => unreachable!(),
        }
    }
}

impl<const BITS: usize> ops::Shl<usize> for BitString<BITS> {
    type Output = Self;
    fn shl(self, amount: usize) -> Self::Output {
        debug_assert!(amount <= BITS, "amount = {amount}, BITS = {BITS}");
        Self {
            bits: self.bits.unbounded_shl(amount as u32),
        }
    }
}

impl<const BITS: usize> ops::ShlAssign<usize> for BitString<BITS> {
    fn shl_assign(&mut self, amount: usize) { *self = *self << amount; }
}
