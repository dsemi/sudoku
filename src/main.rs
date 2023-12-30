use ahash::{AHashMap, AHashSet};
use once_cell::sync::Lazy;
use std::fmt::{Debug, Display, Error, Formatter};
use std::ops::{Index, IndexMut};
use std::time::Instant;

type Idx = (usize, usize);

#[derive(Copy, Clone, Eq, PartialEq)]
struct Sq(u16);

impl Sq {
    #[inline]
    fn len(&self) -> u32 {
        self.0.count_ones()
    }

    #[inline]
    fn val(&self) -> u32 {
        assert_eq!(self.len(), 1);
        self.0.trailing_zeros()
    }

    fn has(&self, b: u32) -> bool {
        self.0 & 1 << b != 0
    }

    fn remove(&mut self, b: u32) {
        self.0 &= !(1 << b);
    }

    fn vals(&self) -> impl Iterator<Item = u32> {
        Bits { n: self.0 }
    }
}

pub struct Bits {
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

const SQ_SIZE: usize = 3;
const SIZE: usize = 9;
const VALS: [usize; SIZE] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const ANY: Sq = {
    let mut i = 0;
    let mut n = 0;
    while i < SIZE {
        n |= 1 << VALS[i];
        i += 1;
    }
    Sq(n)
};
const ROWS: [usize; SIZE] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const COLS: [usize; SIZE] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const SQUARES: [Idx; SIZE * SIZE] = {
    // MaybeUninit would be better, but you can't write in const.
    let mut squares = [(0, 0); SIZE * SIZE];
    let mut r = 0;
    while r < SIZE {
        let mut c = 0;
        while c < SIZE {
            squares[r * SIZE + c] = (ROWS[r], COLS[c]);
            c += 1;
        }
        r += 1;
    }
    squares
};
const UNITS: [[Idx; SIZE]; SIZE * 3] = {
    // MaybeUninit would be better, but you can't write in const.
    let mut unit_list = [[(0, 0); SIZE]; SIZE * 3];
    let mut i = 0;
    let mut c = 0;
    while c < SIZE {
        let mut r = 0;
        while r < SIZE {
            unit_list[i][r] = (ROWS[r], COLS[c]);
            r += 1;
        }
        i += 1;
        c += 1;
    }
    let mut r = 0;
    while r < SIZE {
        let mut c = 0;
        while c < SIZE {
            unit_list[i][c] = (ROWS[r], COLS[c]);
            c += 1;
        }
        i += 1;
        r += 1;
    }
    let mut r = 0;
    while r < SIZE {
        let mut c = 0;
        while c < SIZE {
            let mut id = 0;
            let mut rd = 0;
            while rd < SQ_SIZE {
                let mut cd = 0;
                while cd < SQ_SIZE {
                    unit_list[i][id] = (ROWS[r + rd], COLS[c + cd]);
                    id += 1;
                    cd += 1;
                }
                rd += 1;
            }
            i += 1;
            c += SQ_SIZE;
        }
        r += SQ_SIZE;
    }
    unit_list
};

static UNIT_TBL: Lazy<AHashMap<Idx, Vec<[Idx; SIZE]>>> = Lazy::new(|| {
    SQUARES
        .into_iter()
        .map(|s| (s, UNITS.into_iter().filter(|u| u.contains(&s)).collect()))
        .collect()
});

static PEERS: Lazy<AHashMap<Idx, AHashSet<Idx>>> = Lazy::new(|| {
    SQUARES
        .into_iter()
        .map(|s| {
            let peers = UNIT_TBL[&s]
                .iter()
                .flatten()
                .copied()
                .filter(|&i| i != s)
                .collect();
            (s, peers)
        })
        .collect()
});

#[derive(Clone, Eq, PartialEq)]
struct Grid {
    elems: [[Sq; SIZE]; SIZE],
}

impl Index<Idx> for Grid {
    type Output = Sq;

    fn index(&self, (r, c): Idx) -> &Self::Output {
        &self.elems[r - 1][c - 1]
    }
}

impl IndexMut<Idx> for Grid {
    fn index_mut(&mut self, (r, c): Idx) -> &mut Self::Output {
        &mut self.elems[r - 1][c - 1]
    }
}

impl Grid {
    fn new() -> Self {
        Self {
            elems: [[ANY; SIZE]; SIZE],
        }
    }

    fn parse(input: &str, for_display: bool) -> Option<Self> {
        let chars: Vec<_> = input
            .chars()
            .filter(|&c| c.is_ascii_digit() || c == '.')
            .collect();
        assert_eq!(chars.len(), SIZE * SIZE);
        let mut grid = Self::new();
        for (s, c) in SQUARES.into_iter().zip(chars) {
            if let Some(d) = c.to_digit(10).filter(|&d| d > 0) {
                if for_display {
                    grid[s] = Sq(1 << d);
                } else if !grid.assign(s, d) {
                    return None;
                }
            }
        }
        Some(grid)
    }

    fn pretty(&self, f: &mut Formatter<'_>, show_opts: bool) -> Result<(), Error> {
        let width = SQUARES
            .into_iter()
            .map(|s| if show_opts { self[s].len() } else { 1 })
            .max()
            .unwrap() as usize
            + 1;
        // False positive for non-Copy types.
        // https://github.com/rust-lang/rust-clippy/issues/11958
        #[allow(clippy::useless_vec)]
        let line = vec![vec!['-'; width * SQ_SIZE].into_iter().collect::<String>(); SIZE / SQ_SIZE]
            .join("+");
        for r in ROWS {
            for c in COLS {
                if self[(r, c)].len() == 1 {
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
                if c != 0 && c != SIZE && c % SQ_SIZE == 0 {
                    write!(f, "|")?;
                }
            }
            writeln!(f)?;
            if r != 0 && r != SIZE && r % SQ_SIZE == 0 {
                writeln!(f, "{line}")?;
            }
        }
        writeln!(f)
    }

    fn assign(&mut self, i: Idx, d: u32) -> bool {
        let mut other_vals = self[i];
        other_vals.remove(d);
        other_vals.vals().all(|d| self.eliminate(i, d))
    }

    fn eliminate(&mut self, i: Idx, d: u32) -> bool {
        if !self[i].has(d) {
            return true;
        }
        self[i].remove(d);
        if self[i].len() == 0 {
            return false;
        } else if self[i].len() == 1 {
            let d = self[i].val();
            if !PEERS[&i].iter().all(|&i| self.eliminate(i, d)) {
                return false;
            }
        }
        for &u in &UNIT_TBL[&i] {
            let dplaces: Vec<_> = u.into_iter().filter(|&p| self[p].has(d)).collect();
            if dplaces.is_empty() || dplaces.len() == 1 && !self.assign(dplaces[0], d) {
                return false;
            }
        }
        true
    }

    fn search(self) -> Option<Self> {
        if SQUARES.into_iter().all(|s| self[s].len() == 1) {
            return Some(self);
        }
        let (_, s) = SQUARES
            .into_iter()
            .map(|s| (self[s].len(), s))
            .filter(|&(l, _)| l > 1)
            .min()
            .unwrap();
        // Finds first solution.
        self[s].vals().find_map(|d| {
            let mut grid = self.clone();
            if grid.assign(s, d) {
                grid.search()
            } else {
                None
            }
        })
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.pretty(f, false)
    }
}

impl Debug for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.pretty(f, true)
    }
}

fn time_solve(input: &str) {
    let orig_grid = Grid::parse(input, true).unwrap();
    println!("{orig_grid}");
    let start = Instant::now();
    // Initial constraining.
    let Some(grid) = Grid::parse(input, false) else {
        println!("Invalid initial grid configuration\n");
        return;
    };
    let ans = grid.search();
    let elapsed = start.elapsed();
    let t: f64 = elapsed.as_millis() as f64 / 1000.0;
    if let Some(sol) = ans {
        println!("{sol}");
    } else {
        println!("No solution found");
    }
    println!("{:.3} seconds\n", t)
}

const GRID1: &str = "
8....42..
3...5..6.
5...32...
.......42
.21...38.
47.......
...39...6
.8..7...5
..65....9";

const GRID2: &str = "
85...24..
72......9
..4......
...1.7..2
3.5...9..
.4.......
....8..7.
.17......
....36.4.";

const GRID3: &str = "
..53.....
8......2.
.7..1.5..
4....53..
.1..7...6
..32...8.
.6.5....9
..4....3.
.....97..";

const GRID4: &str = "
....6..8.
.2.......
..1......
.7....1.2
5...3....
......4..
..42.1...
3..7..6..
.......5.";

const GRID5: &str = "
.........
.........
.........
384......
.........
.........
.........
.........
........2";

const GRID6: &str = "
7.1.3....
.8.7.6...
..3.5.9..
...4.2.9.
....7.1.5
.....5.8.
1.....3.9
.3.....6.
9.5.....1";

fn main() {
    for grid in [GRID1, GRID2, GRID3, GRID4, GRID5, GRID6] {
        time_solve(grid);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unit_tbl_size() {
        // Each square in 3 units: row, col, and square.
        for v in UNIT_TBL.values() {
            assert_eq!(v.len(), 3);
        }
    }

    #[test]
    fn peer_size() {
        // Three directions of SIZE (row, col, square)
        // Counts row and col within square twice, so subtract SQ_SIZE twice.
        // Subtract 1 so the key isn't included.
        const PEER_SIZE: usize = SIZE * 3 - SQ_SIZE - SQ_SIZE - 1;
        for v in PEERS.values() {
            assert_eq!(v.len(), PEER_SIZE);
        }
    }

    fn grid_eq(expected: &str, actual: &str) {
        let exp = Grid::parse(expected, true).expect("expected malformed");
        let act = Grid::parse(actual, false).expect("actual malformed");
        let act = act.search().expect("no solution found");
        assert_eq!(exp, act);
    }

    #[test]
    fn grid1() {
        let sol = "897614253
                   312859764
                   564732918
                   953187642
                   621945387
                   478263591
                   745391826
                   289476135
                   136528479";
        grid_eq(sol, GRID1);
    }

    #[test]
    fn grid2() {
        let sol = "859612437
                   723854169
                   164379528
                   986147352
                   375268914
                   241593786
                   432981675
                   617425893
                   598736241";
        grid_eq(sol, GRID2);
    }

    #[test]
    fn grid3() {
        let sol = "145327698
                   839654127
                   672918543
                   496185372
                   218473956
                   753296481
                   367542819
                   984761235
                   521839764";
        grid_eq(sol, GRID3);
    }

    #[test]
    fn grid4() {
        let sol = "947165283
                   823974516
                   651328947
                   478596132
                   516432879
                   239817465
                   764251398
                   385749621
                   192683754";
        grid_eq(sol, GRID4);
    }

    #[test]
    fn grid5() {
        let sol = "836972415
                   971345826
                   425618937
                   384256791
                   167489253
                   259137684
                   612594378
                   548723169
                   793861542";
        grid_eq(sol, GRID5);
    }

    #[test]
    fn grid6() {
        let sol = "721934658
                   589726413
                   463851972
                   358412796
                   294678135
                   617395284
                   146587329
                   832149567
                   975263841";
        grid_eq(sol, GRID6);
    }
}
