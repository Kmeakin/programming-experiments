use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct BitString {
    bits: u64,
}

impl fmt::Binary for BitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "0b{:064b}", self.bits) }
}

impl fmt::UpperHex for BitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "0x{:016X}", self.bits) }
}

impl fmt::LowerHex for BitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "0x{:016x}", self.bits) }
}

impl fmt::Debug for BitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BitString")
            .field("bin", &fmt::from_fn(|f| fmt::Binary::fmt(&self, f)))
            .field("hex", &fmt::from_fn(|f| fmt::UpperHex::fmt(&self, f)))
            .finish()
    }
}

impl BitString {
    #[must_use]
    #[inline]
    pub fn new(bits: u64) -> Self { Self { bits } }

    #[must_use]
    #[inline]
    pub fn first_set(&self) -> Option<usize> {
        if self.bits == 0 {
            None
        } else {
            Some(self.bits.leading_zeros() as usize)
        }
    }

    #[must_use]
    #[inline]
    pub fn last_set(&self) -> Option<usize> {
        if self.bits == 0 {
            None
        } else {
            Some(63 - self.bits.trailing_zeros() as usize)
        }
    }

    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool { self.bits == 0 }

    #[must_use]
    #[inline]
    pub fn clear_upto(self, pos: usize) -> Self {
        debug_assert!(pos <= 64);
        Self {
            bits: self.bits & (u64::MAX.unbounded_shr(pos as u32)),
        }
    }

    #[must_use]
    #[inline]
    pub fn peek(self) -> Option<(usize, Self)> {
        let pos = self.first_set()?;
        Some((pos, self.clear_upto(pos + 1)))
    }

    #[inline]
    pub fn matches(self) -> impl Iterator<Item = usize> {
        let mut this = self;
        std::iter::from_fn(move || {
            let (pos, next) = this.peek()?;
            this = next;
            Some(pos)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_set() {
        assert_eq!(BitString::new(0x0000_0000_0000_0000).first_set(), None);
        assert_eq!(BitString::new(0x8000_0000_0000_0000).first_set(), Some(0));
        assert_eq!(BitString::new(0x7000_0000_0000_0000).first_set(), Some(1));
        assert_eq!(BitString::new(0x1000_0000_0000_0000).first_set(), Some(3));
        assert_eq!(BitString::new(0x0001_0000_0000_0000).first_set(), Some(15));
        assert_eq!(BitString::new(0x0000_0001_0000_0000).first_set(), Some(31));
        assert_eq!(BitString::new(0x0000_0000_0001_0000).first_set(), Some(47));
        assert_eq!(BitString::new(0x0000_0000_0000_0001).first_set(), Some(63));
    }

    #[test]
    fn clear_upto() {
        assert_eq!(
            BitString::new(0x0000_0000_0000_0000).clear_upto(0),
            BitString::new(0x0000_0000_0000_0000)
        );
        assert_eq!(
            BitString::new(0x8000_0000_0000_0000).clear_upto(0),
            BitString::new(0x8000_0000_0000_0000)
        );
        assert_eq!(
            BitString::new(0x8000_0000_0000_0000).clear_upto(1),
            BitString::new(0x0000_0000_0000_0000)
        );
        assert_eq!(
            BitString::new(0x0000_0000_0000_0001).clear_upto(63),
            BitString::new(0x0000_0000_0000_0001)
        );
        assert_eq!(
            BitString::new(0x0000_0000_0000_0001).clear_upto(64),
            BitString::new(0x0000_0000_0000_0000)
        );
    }

    #[test]
    #[cfg(false)]
    fn matches() {
        let matches = |b| BitString::new(b).matches().collect::<Vec<_>>();
        assert_eq!(matches(0x0000_0000_0000_0000), []);
        assert_eq!(matches(0x8000_0000_0000_0000), [0]);
        assert_eq!(matches(0x8000_0000_0000_0000), [0]);
        assert_eq!(matches(0x0000_0000_0000_0001), [63]);
        assert_eq!(matches(u64::MAX), (0..64).collect::<Vec<_>>());
    }
}
