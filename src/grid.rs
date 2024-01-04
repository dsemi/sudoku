use crate::set::Set;
use std::fmt::{Debug, Display, Error, Formatter};
use std::ops::{Index, IndexMut};

type Idx = (usize, usize);

#[derive(Clone, Eq, PartialEq)]
pub struct Grid<const SIZE: usize> {
    elems: [[Set; SIZE]; SIZE],
}

impl<const SIZE: usize> Index<Idx> for Grid<SIZE> {
    type Output = Set;

    fn index(&self, (r, c): Idx) -> &Self::Output {
        &self.elems[r - 1][c - 1]
    }
}

impl<const SIZE: usize> IndexMut<Idx> for Grid<SIZE> {
    fn index_mut(&mut self, (r, c): Idx) -> &mut Self::Output {
        &mut self.elems[r - 1][c - 1]
    }
}

impl<const SIZE: usize> Grid<SIZE> {
    pub fn new() -> Self {
        Self {
            elems: [[Set::full(SIZE); SIZE]; SIZE],
        }
    }

    fn pretty_print(&self, f: &mut Formatter<'_>, show_opts: bool) -> Result<(), Error> {
        let width = self
            .elems
            .iter()
            .flat_map(|row| row.iter().map(|v| if show_opts { v.card() } else { 1 }))
            .max()
            .unwrap() as usize
            + 1;
        // isqrt is in nightly.
        let box_size = (SIZE as f64).sqrt() as usize;
        // False positive for non-Copy types.
        // https://github.com/rust-lang/rust-clippy/issues/11958
        #[allow(clippy::useless_vec)]
        let line =
            vec![vec!['-'; width * box_size].into_iter().collect::<String>(); SIZE / box_size]
                .join("+");
        for r in 1..=SIZE {
            for c in 1..=SIZE {
                if self[(r, c)].card() == 1 {
                    write!(f, "{:^width$}", self[(r, c)].val())?;
                } else if show_opts {
                    let v: String = self[(r, c)]
                        .vals()
                        .map(|d| char::from_digit(d, 10).unwrap())
                        .collect();
                    write!(f, "{:^width$}", v)?;
                } else {
                    write!(f, "{:^width$}", '.')?;
                }
                if c != SIZE && c % box_size == 0 {
                    write!(f, "|")?;
                }
            }
            writeln!(f)?;
            if r != SIZE && r % box_size == 0 {
                writeln!(f, "{line}")?;
            }
        }
        writeln!(f)
    }
}

impl<const SIZE: usize> Display for Grid<SIZE> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.pretty_print(f, false)
    }
}

impl<const SIZE: usize> Debug for Grid<SIZE> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.pretty_print(f, true)
    }
}
