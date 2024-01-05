use ahash::{AHashMap, AHashSet};
use once_cell::sync::Lazy;
use std::cmp::{max, min};
use std::time::Instant;

mod set;
use set::Set;

mod grid;
use grid::Grid;

type Idx = (usize, usize);

static PARTITIONS: Lazy<AHashMap<(u32, usize), Vec<Set>>> = Lazy::new(|| {
    let mut partitions = AHashMap::new();
    for size in 1..=9 {
        let mut buf = vec![0; size + 1];
        for sum in 1..=45 {
            let mut partition = Vec::new();
            let mut stack = vec![(size, 0, sum)];
            while let Some((n, y, t)) = stack.pop() {
                buf[n] = y;
                if n == 1 {
                    if t > y && t <= 9 {
                        buf[0] = t;
                        partition.push(Set::of(&buf[..buf.len() - 1]));
                    }
                    continue;
                }
                for x in max(0, y) + 1..=min(9, t) {
                    stack.push((n - 1, x, t - x));
                }
            }
            partitions.insert((sum, size), partition);
        }
    }
    partitions
});

// Maybe undo this and inline back into peers
struct Unit {
    required_vals: Set,
    locations: Vec<Idx>,
}

struct Cage {
    size: usize,
    sum: u32,
    locations: Vec<Idx>,
}

#[derive(Default)]
struct Rules<const SIZE: usize> {
    squares: Vec<Idx>,
    // Can probably speed up all of the lookups by having a grid of the same size as the sudoku board, holding the values of the maps.

    // Map from each square to all of the units that contain it.
    // A unit must contain all values exactly once.
    units: AHashMap<Idx, Vec<Unit>>,
    // Map from each square to all of its peers.
    // A peer of a square must have a distinct value from that square.
    peers: AHashMap<Idx, AHashSet<Idx>>,
    // All digits in a cage must be unique (handled by adding to peers).
    // Additionally, if the cage has a sum, the sum of the digits in the cage must match.
    cages: AHashMap<Idx, Cage>,
    // Any restriction around non-consecutive digits.
    non_consec: AHashMap<Idx, AHashSet<Idx>>,
}

impl<const SIZE: usize> Rules<SIZE> {
    fn add_cage(&mut self, locations: &[Idx]) {
        for &i in locations {
            for &j in locations {
                if i != j {
                    let e = self.peers.entry(i).or_default();
                    e.insert(j);
                }
            }
        }
    }

    fn add_cage_with_sum(&mut self, sum: u32, locations: &[Idx]) {
        self.add_cage(locations);
        // Maybe switch to reference.
        for &i in locations {
            self.cages.insert(
                i,
                Cage {
                    size: locations.len(),
                    sum,
                    locations: locations.to_vec(),
                },
            );
        }
    }

    fn parse_grid(&self, input: &str) -> Option<Grid<SIZE>> {
        let chars: Vec<_> = input
            .chars()
            .filter(|&c| c.is_ascii_digit() || c == '.')
            .collect();
        assert_eq!(chars.len(), SIZE * SIZE);
        let mut grid = Grid::new();
        for &i in &self.squares {
            if let Some(cage) = self.cages.get(&i) {
                let poss = PARTITIONS[&(cage.sum, cage.size)]
                    .iter()
                    .fold(Set::default(), |acc, &b| acc | b);
                let to_elim = grid[i] & !poss;
                for d in to_elim.vals() {
                    for &i in &cage.locations {
                        if !self.eliminate(&mut grid, i, d) {
                            return None;
                        }
                    }
                }
            }
        }
        for (&i, v) in self.squares.iter().zip(chars) {
            if let Some(d) = v.to_digit(10).filter(|&d| d > 0) {
                if !self.assign(&mut grid, i, d) {
                    return None;
                }
            }
        }
        Some(grid)
    }

    fn assign(&self, grid: &mut Grid<SIZE>, i: Idx, d: u32) -> bool {
        grid[i].remove(d).vals().all(|d| self.eliminate(grid, i, d))
    }

    fn eliminate(&self, grid: &mut Grid<SIZE>, i: Idx, d: u32) -> bool {
        if !grid[i].has(d) {
            return true;
        }
        grid[i] = grid[i].remove(d);
        if grid[i].card() == 0 {
            return false;
        } else if grid[i].card() == 1 {
            let d = grid[i].val();
            if !self.peers[&i].iter().all(|&i| self.eliminate(grid, i, d)) {
                return false;
            }
            if let Some(non_consec_peers) = self.non_consec.get(&i) {
                let mut to_elim = [d - 1, d + 1]
                    .into_iter()
                    .filter(|v| (1..=SIZE as u32).contains(v));
                if !to_elim.all(|d| non_consec_peers.iter().all(|&i| self.eliminate(grid, i, d))) {
                    return false;
                }
            }
            if let Some(cage) = self.cages.get(&i) {
                let (resolved, unresolved): (Vec<_>, Vec<_>) = cage
                    .locations
                    .iter()
                    .copied()
                    .partition(|&i| grid[i].card() == 1);
                if !unresolved.is_empty() {
                    let (resolved_sum, resolved_set) =
                        resolved.iter().fold((0, Set::default()), |(sum, set), &i| {
                            (sum + grid[i].val(), set | grid[i])
                        });
                    let unresolved_sum = cage.sum - resolved_sum;
                    let valid_neighbs = PARTITIONS[&(cage.sum, cage.size)]
                        .iter()
                        .filter(|set| set.has_all(resolved_set))
                        .fold(Set::default(), |a, &b| a | b);
                    match PARTITIONS.get(&(unresolved_sum, unresolved.len())) {
                        None => return false,
                        Some(parts) => {
                            let mut valid_set = parts.iter().fold(Set::default(), |a, &b| a | b);
                            valid_set = valid_set & valid_neighbs;
                            let to_remove = Set::full(SIZE) & !valid_set;
                            for d in to_remove.vals() {
                                for &loc in &unresolved {
                                    if !self.eliminate(grid, loc, d) {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                } else if cage.sum != resolved.iter().map(|&i| grid[i].val()).sum() {
                    return false;
                }
            }
        }
        for u in &self.units[&i] {
            if !u.required_vals.has(d) {
                continue;
            }
            let dplaces: Vec<_> = u
                .locations
                .iter()
                .cloned()
                .filter(|&p| grid[p].has(d))
                .collect();
            if dplaces.is_empty() || dplaces.len() == 1 && !self.assign(grid, dplaces[0], d) {
                return false;
            }
        }
        true
    }

    fn search(&self, grid: Grid<SIZE>) -> Option<Grid<SIZE>> {
        if self.squares.iter().all(|&s| grid[s].card() == 1) {
            return Some(grid);
        }

        let s = self
            .squares
            .iter()
            .copied()
            .filter(|&i| grid[i].card() > 1)
            .min_by_key(|&i| grid[i].card())
            .unwrap();
        grid[s].vals().find_map(|d| {
            let mut grid = grid.clone();
            if self.assign(&mut grid, s, d) {
                self.search(grid)
            } else {
                None
            }
        })
    }

    fn time_solve(&self, input: &str) {
        let orig_grid = parse_grid_no_rules::<SIZE>(input);
        println!("{orig_grid}");

        let start = Instant::now();
        // Initial constraining.
        let Some(grid) = self.parse_grid(input) else {
            println!("Invalid initial grid configuration\n");
            return;
        };
        println!("{grid:?}");
        let ans = self.search(grid);
        let elapsed = start.elapsed();
        let t: f64 = elapsed.as_millis() as f64 / 1000.0;
        if let Some(sol) = ans {
            println!("{sol}");
        } else {
            println!("No solution found");
        }
        println!("{:.3} seconds\n", t)
    }
}

fn parse_grid_no_rules<const SIZE: usize>(input: &str) -> Grid<SIZE> {
    let chars: Vec<_> = input
        .chars()
        .filter(|&c| c.is_ascii_digit() || c == '.')
        .collect();
    assert_eq!(chars.len(), SIZE * SIZE);
    let mut grid = Grid::new();
    for (i, v) in chars.into_iter().enumerate() {
        let (r, c) = (i / SIZE, i % SIZE);
        if let Some(d) = v.to_digit(10).filter(|&d| d > 0) {
            grid[(r + 1, c + 1)] = Set::default().insert(d);
        }
    }
    grid
}

const NORMAL_SIZE: usize = 9;

fn normal() -> Rules<NORMAL_SIZE> {
    let mut rules = Rules::default();
    // isqrt is in nightly.
    let box_size = (NORMAL_SIZE as f64).sqrt() as usize;
    rules.squares = (1..=NORMAL_SIZE)
        .flat_map(|r| (1..=NORMAL_SIZE).map(move |c| (r, c)))
        .collect();
    let mut all_units: Vec<Vec<Idx>> = Vec::new();
    for r in 1..=NORMAL_SIZE {
        all_units.push((1..=NORMAL_SIZE).map(|c| (r, c)).collect());
    }
    for c in 1..=NORMAL_SIZE {
        all_units.push((1..=NORMAL_SIZE).map(|r| (r, c)).collect());
    }
    for r in (1..=NORMAL_SIZE).step_by(box_size) {
        for c in (1..=NORMAL_SIZE).step_by(box_size) {
            let mut bx = Vec::with_capacity(NORMAL_SIZE);
            for dr in 0..box_size {
                for dc in 0..box_size {
                    bx.push((r + dr, c + dc));
                }
            }
            all_units.push(bx);
        }
    }
    rules.units = rules
        .squares
        .iter()
        .map(|s| {
            let units: Vec<_> = all_units
                .iter()
                .filter(|u| u.contains(s))
                .cloned()
                .map(|u| Unit {
                    required_vals: Set::full(NORMAL_SIZE),
                    locations: u,
                })
                .collect();
            (*s, units)
        })
        .collect();
    rules.peers = rules
        .squares
        .iter()
        .map(|&s| {
            let peers = rules.units[&s]
                .iter()
                .flat_map(|unit| &unit.locations)
                .copied()
                .filter(|&v| v != s)
                .collect();
            (s, peers)
        })
        .collect();
    rules
}

fn unique_knight_moves<const SIZE: usize>(rules: &mut Rules<SIZE>) {
    for &(r, c) in &rules.squares {
        let knight_moves = [
            (r - 2, c - 1),
            (r - 2, c + 1),
            (r - 1, c - 2),
            (r - 1, c + 2),
            (r + 1, c - 2),
            (r + 1, c + 2),
            (r + 2, c - 1),
            (r + 2, c + 1),
        ];
        for pos in knight_moves {
            if (1..=9).contains(&pos.0) && (1..=9).contains(&pos.1) {
                rules.peers.entry((r, c)).or_default().insert(pos);
            }
        }
    }
}

fn unique_king_moves<const SIZE: usize>(rules: &mut Rules<SIZE>) {
    for &(r, c) in &rules.squares {
        let king_moves = [
            (r - 1, c - 1),
            (r - 1, c),
            (r - 1, c + 1),
            (r, c - 1),
            (r, c + 1),
            (r + 1, c - 1),
            (r + 1, c),
            (r + 1, c + 1),
        ];
        for pos in king_moves {
            if (1..=9).contains(&pos.0) && (1..=9).contains(&pos.1) {
                rules.peers.entry((r, c)).or_default().insert(pos);
            }
        }
    }
}

fn non_consecutive_orthogonal<const SIZE: usize>(rules: &mut Rules<SIZE>) {
    for &(r, c) in &rules.squares {
        let ortho = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)];
        for pos in ortho {
            if (1..=9).contains(&pos.0) && (1..=9).contains(&pos.1) {
                rules.non_consec.entry((r, c)).or_default().insert(pos);
            }
        }
    }
}

fn main() {
    let hard20: [&str; 20] = [
        "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9",
        ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...",
        ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..",
        "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........",
        "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8",
        "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1",
        ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....",
        "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8",
        "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.",
        "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.",
        "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..",
        "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7",
        "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........",
        "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........",
        "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....",
        "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3",
        ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...",
        ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..",
        "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1",
        ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....",
    ];
    let rules = normal();
    for j in 0..20 {
        rules.time_solve(hard20[j]);
        // println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn normal_grid_eq(expected: &str, actual: &str) {
        let rules = normal();
        let exp = rules.parse_grid(expected).expect("expected malformed");
        let act = rules.parse_grid(actual).expect("actual malformed");
        let act = rules.search(act).expect("no solution found");
        assert_eq!(exp, act);
    }

    #[test]
    fn grid1() {
        let grd = "8....42..
                   3...5..6.
                   5...32...
                   .......42
                   .21...38.
                   47.......
                   ...39...6
                   .8..7...5
                   ..65....9";
        let sol = "897614253
                   312859764
                   564732918
                   953187642
                   621945387
                   478263591
                   745391826
                   289476135
                   136528479";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn grid2() {
        let grd = "85...24..
                   72......9
                   ..4......
                   ...1.7..2
                   3.5...9..
                   .4.......
                   ....8..7.
                   .17......
                   ....36.4.";
        let sol = "859612437
                   723854169
                   164379528
                   986147352
                   375268914
                   241593786
                   432981675
                   617425893
                   598736241";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn grid3() {
        let grd = "..53.....
                   8......2.
                   .7..1.5..
                   4....53..
                   .1..7...6
                   ..32...8.
                   .6.5....9
                   ..4....3.
                   .....97..";
        let sol = "145327698
                   839654127
                   672918543
                   496185372
                   218473956
                   753296481
                   367542819
                   984761235
                   521839764";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn grid4() {
        let grd = "....6..8.
                   .2.......
                   ..1......
                   .7....1.2
                   5...3....
                   ......4..
                   ..42.1...
                   3..7..6..
                   .......5.";
        let sol = "947165283
                   823974516
                   651328947
                   478596132
                   516432879
                   239817465
                   764251398
                   385749621
                   192683754";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn grid5() {
        let grd = ".........
                   .........
                   .........
                   384......
                   .........
                   .........
                   .........
                   .........
                   ........2";
        let sol = "836972415
                   971345826
                   425618937
                   384256791
                   167489253
                   259137684
                   612594378
                   548723169
                   793861542";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn grid6() {
        let grd = "7.1.3....
                   .8.7.6...
                   ..3.5.9..
                   ...4.2.9.
                   ....7.1.5
                   .....5.8.
                   1.....3.9
                   .3.....6.
                   9.5.....1";
        let sol = "721934658
                   589726413
                   463851972
                   358412796
                   294678135
                   617395284
                   146587329
                   832149567
                   975263841";
        normal_grid_eq(sol, grd);
    }

    #[test]
    fn miracle() {
        let grd = ".........
                   .........
                   .........
                   .........
                   ..1......
                   ......2..
                   .........
                   .........
                   .........";
        let sol = "483726159
                   726159483
                   159483726
                   837261594
                   261594837
                   594837261
                   372615948
                   615948372
                   948372615";

        let mut rules = normal();
        unique_knight_moves(&mut rules);
        unique_king_moves(&mut rules);
        non_consecutive_orthogonal(&mut rules);
        let exp = rules.parse_grid(sol).expect("expected malformed");
        let act = rules.parse_grid(grd).expect("actual malformed");
        let act = rules.search(act).expect("no solution found");
        assert_eq!(exp, act);
    }

    #[test]
    fn killer() {
        let grd = ".........
                   .........
                   .........
                   .........
                   .........
                   .........
                   .........
                   .........
                   .........";
        let sol = "671459328
                   285736491
                   439821765
                   156398247
                   928147653
                   743562189
                   594213876
                   367984512
                   812675934";
        let mut rules = normal();
        // Would be nice to parse these without needing to add manually.
        rules.add_cage_with_sum(15, &[(1, 1), (1, 2), (2, 1)]);
        rules.add_cage_with_sum(5, &[(1, 3), (1, 4)]);
        rules.add_cage_with_sum(27, &[(1, 5), (2, 5), (3, 5), (4, 5), (4, 6)]);
        rules.add_cage_with_sum(
            45,
            &[
                (1, 6),
                (2, 6),
                (1, 7),
                (1, 8),
                (1, 9),
                (2, 9),
                (3, 9),
                (4, 9),
                (4, 8),
            ],
        );
        rules.add_cage_with_sum(13, &[(2, 7), (2, 8)]);
        rules.add_cage_with_sum(13, &[(3, 7), (3, 8)]);
        rules.add_cage_with_sum(5, &[(3, 1), (4, 1)]);
        rules.add_cage_with_sum(23, &[(4, 4), (5, 4), (5, 3), (5, 2), (5, 1)]);
        rules.add_cage_with_sum(23, &[(5, 6), (6, 6), (5, 7), (5, 8), (5, 9)]);
        rules.add_cage_with_sum(
            45,
            &[
                (6, 1),
                (6, 2),
                (7, 1),
                (8, 1),
                (9, 1),
                (9, 2),
                (9, 3),
                (9, 4),
                (8, 4),
            ],
        );
        rules.add_cage_with_sum(27, &[(6, 4), (6, 5), (7, 5), (8, 5), (9, 5)]);
        rules.add_cage_with_sum(13, &[(7, 2), (7, 3)]);
        rules.add_cage_with_sum(13, &[(8, 2), (8, 3)]);
        rules.add_cage_with_sum(13, &[(7, 7), (8, 7)]);
        rules.add_cage_with_sum(8, &[(7, 8), (8, 8)]);
        let exp = rules.parse_grid(sol).expect("expected malformed");
        let act = rules.parse_grid(grd).expect("actual malformed");
        let act = rules.search(act).expect("no solution found");
        assert_eq!(exp, act);
    }
}
