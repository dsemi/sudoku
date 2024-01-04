use std::ops::{BitAnd, BitOr, Not};

#[derive(Copy, Clone, Default, Eq, PartialEq)]
pub struct Set(u16);

impl Set {
    pub const fn of(ns: &[u32]) -> Self {
        let mut i = 0;
        let mut n = 0;
        while i < ns.len() {
            n |= 1 << ns[i];
            i += 1;
        }
        Set(n)
    }

    #[inline]
    pub fn full(bits: usize) -> Self {
        (1..=bits).fold(Set::default(), |acc, a| acc.insert(a as u32))
    }

    #[inline]
    pub fn card(&self) -> u32 {
        self.0.count_ones()
    }

    #[inline]
    pub fn val(&self) -> u32 {
        assert_eq!(self.card(), 1);
        self.0.trailing_zeros()
    }

    #[inline]
    pub fn has(&self, b: u32) -> bool {
        self.0 & 1 << b != 0
    }

    #[inline]
    pub fn has_all(&self, b: Set) -> bool {
        self.0 & b.0 == b.0
    }

    #[inline]
    pub fn insert(self, b: u32) -> Self {
        Self(self.0 | 1 << b)
    }

    #[inline]
    pub fn remove(self, b: u32) -> Self {
        Self(self.0 & !(1 << b))
    }

    pub fn vals(&self) -> impl Iterator<Item = u32> {
        Bits { n: self.0 }
    }
}

impl Not for Set {
    type Output = Self;

    fn not(self) -> Self {
        Self(!self.0)
    }
}

impl BitAnd for Set {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitOr for Set {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

struct Bits {
    n: u16,
}

impl Iterator for Bits {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.n == 0 {
            return None;
        }
        let b = self.n.trailing_zeros();
        self.n &= self.n - 1;
        Some(b)
    }
}
