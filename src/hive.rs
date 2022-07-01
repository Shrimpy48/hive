use crate::render::*;
use arrayvec::ArrayVec;
use enum_map::Enum;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::num::ParseIntError;
use std::ops::{Add, Div, Sub};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum, Hash)]
pub enum Colour {
    White,
    Black,
}

impl Colour {
    pub fn next(&self) -> Colour {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Enum, Clone, Copy, Hash)]
pub enum PieceType {
    Queen,
    Beetle,
    Hopper,
    Spider,
    Ant,
    // Ladybug,
    // Mosquito,
    // Pillbug
}

impl fmt::Display for PieceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PieceType::Queen => write!(f, "Q"),
            PieceType::Beetle => write!(f, "B"),
            PieceType::Hopper => write!(f, "G"),
            PieceType::Spider => write!(f, "S"),
            PieceType::Ant => write!(f, "A"),
            // PieceType::Ladybug => write!(f, "L"),
            // PieceType::Mosquito => write!(f, "M"),
            // PieceType::Pillbug => write!(f, "P"),
        }
    }
}

impl FromStr for PieceType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "Q" => Ok(PieceType::Queen),
            "B" => Ok(PieceType::Beetle),
            "G" => Ok(PieceType::Hopper),
            "S" => Ok(PieceType::Spider),
            "A" => Ok(PieceType::Ant),
            // "L" => PieceType::Ladybug,
            // "M" => PieceType::Mosquito,
            // "P" => PieceType::Pillbug,
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum, Hash)]
pub struct Piece {
    pub typ: PieceType,
    pub col: Colour,
}

impl Piece {
    fn put(&self, canvas: &mut Canvas, coord: Coord) {
        let l1 = match self.typ {
            PieceType::Queen => ['B', 'E', 'E'],
            PieceType::Beetle => ['B', 'T', 'L'],
            PieceType::Hopper => ['G', 'H', 'R'],
            PieceType::Spider => ['S', 'P', 'I'],
            PieceType::Ant => ['A', 'N', 'T'],
            // PieceType::Ladybug => ['L', 'D', 'Y'],
            // PieceType::Mosquito => ['M', 'O', 'Z'],
            // PieceType::Pillbug => ['P', 'I', 'L'],
        };
        let l2 = match self.col {
            Colour::White => [' ', 'W', ' '],
            Colour::Black => [' ', 'B', ' '],
        };
        canvas.put_tile(coord, l1, l2);
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut canvas = Canvas::new();
        self.put(&mut canvas, (0, 0));
        f.write_str(&canvas.render())
    }
}

// At most 5 pieces can be stacked (4 beetles on top of another piece).
// With mosquitoes 7 can be stacked.
type PieceStack = ArrayVec<Piece, 5>;
// type PieceStack = ArrayVec<Piece, 7>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Pos {
    pub x: i32,
    pub y: i32,
}

impl Pos {
    fn min_dist(self, b: Pos) -> u32 {
        let Pos { x: ax, y: ay } = self;
        let az = -ax - ay;
        let Pos { x: bx, y: by } = b;
        let bz = -bx - by;
        let dx = (ax - bx).abs();
        let dy = (ay - by).abs();
        let dz = (az - bz).abs();
        dx.max(dy.max(dz)) as u32
    }

    fn go(self, d: Dir) -> Pos {
        let Pos { x, y } = self;
        match d {
            Dir::Up => Pos { x, y: y + 1 },
            Dir::UpRight => Pos { x: x + 1, y },
            Dir::DownRight => Pos { x: x + 1, y: y - 1 },
            Dir::Down => Pos { x, y: y - 1 },
            Dir::DownLeft => Pos { x: x - 1, y },
            Dir::UpLeft => Pos { x: x - 1, y: y + 1 },
        }
    }

    // fn neighbours(self) -> [Pos; 6] {
    //     let Pos { x, y } = self;
    //     [
    //         Pos { x, y: y + 1 },
    //         Pos { x: x + 1, y },
    //         Pos { x: x + 1, y: y - 1 },
    //         Pos { x, y: y - 1 },
    //         Pos { x: x - 1, y },
    //         Pos { x: x - 1, y: y + 1 },
    //     ]
    // }

    fn min_pw(self, other: Pos) -> Pos {
        Pos {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
        }
    }

    fn max_pw(self, other: Pos) -> Pos {
        Pos {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
        }
    }
}

impl Add for Pos {
    type Output = Self;

    fn add(self, other: Pos) -> Pos {
        Pos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Pos {
    type Output = Self;

    fn sub(self, other: Pos) -> Pos {
        Pos {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Div<i32> for Pos {
    type Output = Self;

    fn div(self, i: i32) -> Pos {
        Pos {
            x: self.x / i,
            y: self.y / i,
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl FromStr for Pos {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s
            .trim()
            .trim_matches(|p| p == '(' || p == ')')
            .split(',')
            .collect();

        let x_fromstr = coords[0].parse::<i32>()?;
        let y_fromstr = coords[1].parse::<i32>()?;

        Ok(Pos {
            x: x_fromstr,
            y: y_fromstr,
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct AbsPos(Pos);

impl AbsPos {
    pub fn from_pos(p: Pos) -> AbsPos {
        AbsPos(p)
    }

    pub fn to_rel(self, offset: Pos) -> RelPos {
        RelPos(self.0 - offset)
    }

    fn min_dist(self, b: AbsPos) -> u32 {
        self.0.min_dist(b.0)
    }

    pub fn neighbours(self) -> [AbsPos; 6] {
        let Pos { x, y } = self.0;
        [
            AbsPos(Pos { x, y: y + 1 }),
            AbsPos(Pos { x: x + 1, y }),
            AbsPos(Pos { x: x + 1, y: y - 1 }),
            AbsPos(Pos { x, y: y - 1 }),
            AbsPos(Pos { x: x - 1, y }),
            AbsPos(Pos { x: x - 1, y: y + 1 }),
        ]
    }

    fn go(self, d: Dir) -> AbsPos {
        AbsPos(self.0.go(d))
    }

    fn min_pw(self, other: AbsPos) -> AbsPos {
        AbsPos(self.0.min_pw(other.0))
    }

    fn max_pw(self, other: AbsPos) -> AbsPos {
        AbsPos(self.0.max_pw(other.0))
    }
}

impl fmt::Display for AbsPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for AbsPos {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pos = s.parse::<Pos>()?;
        Ok(AbsPos(pos))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct RelPos(Pos);

impl RelPos {
    // pub fn from_pos(p: Pos) -> RelPos {
    //     RelPos(p)
    // }

    pub fn to_abs(self, offset: Pos) -> AbsPos {
        AbsPos(self.0 + offset)
    }

    // fn min_dist(self, b: RelPos) -> u32 {
    //     self.0.min_dist(b.0)
    // }

    // fn neighbours(self) -> [RelPos; 6] {
    //     let Pos { x, y } = self.0;
    //     [
    //         RelPos(Pos { x, y: y + 1 }),
    //         RelPos(Pos { x: x + 1, y }),
    //         RelPos(Pos { x: x + 1, y: y - 1 }),
    //         RelPos(Pos { x, y: y - 1 }),
    //         RelPos(Pos { x: x - 1, y }),
    //         RelPos(Pos { x: x - 1, y: y + 1 }),
    //     ]
    // }

    // fn go(self, d: Dir) -> RelPos {
    //     RelPos(self.0.go(d))
    // }

    // fn min_pw(self, other: RelPos) -> RelPos {
    //     RelPos(self.0.min_pw(other.0))
    // }

    // fn max_pw(self, other: RelPos) -> RelPos {
    //     RelPos(self.0.max_pw(other.0))
    // }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Dir {
    Up,
    UpRight,
    DownRight,
    Down,
    DownLeft,
    UpLeft,
}

impl Dir {
    fn dirs() -> [Dir; 6] {
        [
            Dir::Up,
            Dir::UpRight,
            Dir::DownRight,
            Dir::Down,
            Dir::DownLeft,
            Dir::UpLeft,
        ]
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct CostItem {
    cost: u32,
    val: AbsPos,
}

impl Ord for CostItem {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.val.cmp(&other.val))
    }
}

impl PartialOrd for CostItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// There are 22 pieces (26 with ladybirds and mosquitoes).
// The board must have space to put all of these in a line without touching the other end.
const BOARD_SIZE: usize = 23;
// const BOARD_SIZE: usize = 27;

// Uses a fixed-size toroidal playing surface.
// The hive can move arbitrarily far, but its size is bounded by the number of pieces,
// so this representation correctly models the hive.
// The origin is placed in the middle of the board so that wrapping occurs infrequently
// as this should benefit caching.
#[derive(Debug, Clone)]
pub struct Hive {
    data: [[PieceStack; BOARD_SIZE]; BOARD_SIZE],
    pub bl: AbsPos,
    pub tr: AbsPos,
}

impl Hive {
    pub fn new() -> Hive {
        Hive {
            data: Default::default(),
            bl: AbsPos(Pos { x: 0, y: 0 }),
            tr: AbsPos(Pos { x: 0, y: 0 }),
        }
    }

    pub fn offset(&self) -> Pos {
        (self.tr.0 + self.bl.0) / 2
    }

    fn get(&self, pos: AbsPos) -> &PieceStack {
        &self.data[((pos.0.x + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
            [((pos.0.y + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
    }

    fn get_mut(&mut self, pos: AbsPos) -> &mut PieceStack {
        &mut self.data[((pos.0.x + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
            [((pos.0.y + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
    }

    pub fn push(&mut self, dst: AbsPos, piece: Piece) {
        self.bl = self.bl.min_pw(dst);
        self.tr = self.tr.max_pw(dst);
        self.get_mut(dst).push(piece);
    }

    pub fn pop(&mut self, src: AbsPos) -> Option<Piece> {
        let s = self.get_mut(src);
        if let Some(piece) = s.pop() {
            if s.is_empty() {
                if src.0.x == self.bl.0.x
                    || src.0.y == self.bl.0.y
                    || src.0.x == self.tr.0.x
                    || src.0.y == self.tr.0.y
                {
                    let (bl, tr) = self.occupied().fold(
                        (
                            AbsPos(Pos {
                                x: i32::MAX,
                                y: i32::MAX,
                            }),
                            AbsPos(Pos {
                                x: i32::MIN,
                                y: i32::MIN,
                            }),
                        ),
                        |(bl, tr), pos| (bl.min_pw(pos), tr.max_pw(pos)),
                    );
                    self.bl = bl;
                    self.tr = tr;
                }
            }
            return Some(piece);
        }
        return None;
    }

    pub fn occupied(&self) -> impl Iterator<Item = AbsPos> + '_ {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| AbsPos(Pos { x, y })))
            .filter(move |p| !self.is_free(*p))
    }

    pub fn tiles(&self) -> impl Iterator<Item = (AbsPos, &Piece)> {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| AbsPos(Pos { x, y })))
            .filter_map(move |p| self.get(p).last().map(|t| (p, t)))
    }

    pub fn all(&self) -> impl Iterator<Item = (AbsPos, usize, &Piece)> {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| AbsPos(Pos { x, y })))
            .flat_map(move |p| self.get(p).iter().enumerate().map(move |(h, v)| (p, h, v)))
    }

    pub fn is_free(&self, p: AbsPos) -> bool {
        self.get(p).is_empty()
    }

    fn count(&self, p: AbsPos) -> usize {
        self.get(p).len()
    }

    pub fn neighbours(&self, p: AbsPos, exclude: Option<AbsPos>) -> impl Iterator<Item = &Piece> {
        p.neighbours().into_iter().filter_map(move |p| {
            if let Some(e) = exclude {
                if p == e {
                    return None;
                }
            }
            self.get(p).last()
        })
    }

    fn are_connected(&self, a: AbsPos, b: AbsPos, excluding: AbsPos) -> bool {
        // Performs an A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists: HashMap<AbsPos, u32> = Default::default();
        dists.insert(a, 0);
        heap.push(CostItem {
            cost: a.min_dist(b),
            val: a,
        });

        while let Some(CostItem { cost, val }) = heap.pop() {
            if val == b {
                return true;
            }

            if cost > *dists.get(&val).unwrap() + val.min_dist(b) {
                // A better path to this node has already been found
                continue;
            }

            for p in val
                .neighbours()
                .into_iter()
                .filter(|&p| !self.is_free(p) && p != excluding)
            {
                let dist = *dists.get(&val).unwrap() + 1;
                let should_push = match dists.get(&p) {
                    None => true,
                    Some(d) => dist < *d,
                };
                if should_push {
                    heap.push(CostItem {
                        cost: dist + p.min_dist(b),
                        val: p,
                    });
                    dists.insert(p, dist);
                }
            }
        }
        return false;
    }

    fn is_structural(&self, p: AbsPos) -> bool {
        if self.count(p) > 1 {
            return false;
        }
        let mut ns = p.neighbours().to_vec();
        ns.dedup_by_key(|n| self.is_free(*n));
        if self.is_free(*ns.first().unwrap()) == self.is_free(*ns.last().unwrap()) {
            ns.pop();
        }
        ns.retain(|n| !self.is_free(*n));
        if ns.len() <= 1 {
            return false;
        }
        let n1 = ns.pop().unwrap();
        for ni in ns {
            if !self.are_connected(n1, ni, p) {
                return true;
            }
        }
        return false;
    }

    pub fn destinations(&self, p: AbsPos, typ: PieceType) -> Vec<AbsPos> {
        if self.is_structural(p) {
            return vec![];
        }
        match typ {
            PieceType::Queen => {
                let ns = p.neighbours();
                let surrounding: Vec<bool> = ns.into_iter().map(|p| self.is_free(p)).collect();
                let mut out = Vec::new();
                for d in 0..6 {
                    if surrounding[d]
                        && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                        && self.neighbours(ns[d], Some(p)).next().is_some()
                    {
                        out.push(ns[d]);
                    }
                }
                return out;
            }
            PieceType::Beetle => {
                let ns = p.neighbours();
                let surrounding: Vec<usize> = ns.into_iter().map(|p| self.count(p)).collect();
                let src_height = self.count(p) - 1;
                let mut out = Vec::new();
                for d in 0..6 {
                    if surrounding[d].max(src_height)
                        >= surrounding[(d + 5) % 6].min(surrounding[(d + 1) % 6])
                        && (src_height > 0 || self.neighbours(ns[d], Some(p)).next().is_some())
                    {
                        out.push(ns[d]);
                    }
                }
                return out;
            }
            PieceType::Hopper => Dir::dirs()
                .into_iter()
                .filter_map(|d| {
                    let mut pos = p.go(d);
                    if self.is_free(pos) {
                        return None;
                    }
                    pos = pos.go(d);
                    while !self.is_free(pos) {
                        pos = pos.go(d);
                    }
                    return Some(pos);
                })
                .collect(),
            PieceType::Ant => {
                let mut visited = HashSet::new();
                let mut to_consider = VecDeque::new();
                visited.insert(p);
                to_consider.push_back(p);
                while let Some(pos) = to_consider.pop_front() {
                    let ns = pos.neighbours();
                    let surrounding: Vec<bool> = ns.into_iter().map(|p| self.is_free(p)).collect();
                    for d in 0..6 {
                        if !visited.contains(&ns[d])
                            && surrounding[d]
                            && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                            && self.neighbours(ns[d], Some(p)).next().is_some()
                        {
                            visited.insert(ns[d]);
                            to_consider.push_back(ns[d]);
                        }
                    }
                }
                visited.into_iter().filter(|&pos| pos != p).collect()
            }
            PieceType::Spider => {
                let mut reach_1 = HashSet::new();
                let mut reach_2 = HashSet::new();
                let mut reach_3 = HashSet::new();
                let ns = p.neighbours();
                let surrounding: Vec<bool> = ns.into_iter().map(|pos| self.is_free(pos)).collect();
                for d in 0..6 {
                    if surrounding[d] && (surrounding[(d + 5) % 6] != surrounding[(d + 1) % 6]) {
                        reach_1.insert(ns[d]);
                    }
                }
                for pos in reach_1.iter() {
                    let ns = pos.neighbours();
                    let surrounding: Vec<bool> = ns
                        .iter()
                        .map(|&pos| if pos == p { true } else { self.is_free(pos) })
                        .collect();
                    for d in 0..6 {
                        if ns[d] != p
                            && !reach_1.contains(&ns[d])
                            && surrounding[d]
                            && (surrounding[(d + 5) % 6] != surrounding[(d + 1) % 6])
                        {
                            reach_2.insert(ns[d]);
                        }
                    }
                }
                for pos in reach_2.iter() {
                    let ns = pos.neighbours();
                    let surrounding: Vec<bool> = ns
                        .iter()
                        .map(|&pos| if pos == p { true } else { self.is_free(pos) })
                        .collect();
                    for d in 0..6 {
                        if ns[d] != p
                            && !reach_1.contains(&ns[d])
                            && !reach_2.contains(&ns[d])
                            && surrounding[d]
                            && (surrounding[(d + 5) % 6] != surrounding[(d + 1) % 6])
                        {
                            reach_3.insert(ns[d]);
                        }
                    }
                }
                reach_3.into_iter().collect()
            }
        }
    }
}

impl fmt::Display for Hive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let offset = self.offset();
        let mut canvas = Canvas::new();
        let mut to_draw: Vec<_> = self.all().collect();
        to_draw.sort_unstable_by_key(|(_, h, _)| *h);
        for (pos, h, piece) in to_draw {
            let pos_offset = pos.to_rel(offset);
            let x = pos_offset.0.x * 6 - h as i32;
            let y = -(pos_offset.0.x * 2 + pos_offset.0.y * 4) - h as i32;
            piece.put(&mut canvas, (x, y));
        }
        f.write_str(&canvas.render())
    }
}
