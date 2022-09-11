use crate::render::*;
use crate::small_arrayvec::SmallArrayVec;
use ahash::AHashSet;
use enum_map::Enum;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::VecDeque;
use std::fmt;
use std::num::ParseIntError;
use std::ops::Index;
use std::ops::IndexMut;
use std::ops::{Add, Div, Sub};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Colour {
    White,
    Black = 0b10000000,
}

impl Colour {
    pub fn next(&self) -> Colour {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

impl fmt::Display for Colour {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::White => write!(f, "white"),
            Self::Black => write!(f, "black"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Enum, Clone, Copy, Hash, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Piece {
    WhiteQueen,
    WhiteBeetle,
    WhiteHopper,
    WhiteSpider,
    WhiteAnt,
    BlackQueen = 0b10000000,
    BlackBeetle,
    BlackHopper,
    BlackSpider,
    BlackAnt,
}

impl Piece {
    pub fn new(typ: PieceType, col: Colour) -> Self {
        (typ as u8 | col as u8).try_into().unwrap()
    }

    pub fn typ(self) -> PieceType {
        (self as u8 & 0b01111111).try_into().unwrap()
    }

    pub fn col(self) -> Colour {
        (self as u8 & 0b10000000).try_into().unwrap()
    }

    fn put(&self, canvas: &mut Canvas, coord: Coord) {
        let l1 = match self.typ() {
            PieceType::Queen => ['B', 'E', 'E'],
            PieceType::Beetle => ['B', 'T', 'L'],
            PieceType::Hopper => ['G', 'H', 'R'],
            PieceType::Spider => ['S', 'P', 'I'],
            PieceType::Ant => ['A', 'N', 'T'],
            // PieceType::Ladybug => ['L', 'D', 'Y'],
            // PieceType::Mosquito => ['M', 'O', 'Z'],
            // PieceType::Pillbug => ['P', 'I', 'L'],
        };
        let l2 = match self.col() {
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
pub(crate) type PieceStack = SmallArrayVec<Piece, 5>;
// type PieceStack = SmallArrayVec<Piece, 7>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct RawPos {
    pub x: i32,
    pub y: i32,
}

impl RawPos {
    fn min_dist(self, b: RawPos) -> u32 {
        let RawPos { x: ax, y: ay } = self;
        let az = -ax - ay;
        let RawPos { x: bx, y: by } = b;
        let bz = -bx - by;
        let dx = (ax - bx).abs();
        let dy = (ay - by).abs();
        let dz = (az - bz).abs();
        dx.max(dy.max(dz)) as u32
    }

    fn go(self, d: Dir) -> RawPos {
        let RawPos { x, y } = self;
        match d {
            Dir::Up => RawPos { x, y: y + 1 },
            Dir::UpRight => RawPos { x: x + 1, y },
            Dir::DownRight => RawPos { x: x + 1, y: y - 1 },
            Dir::Down => RawPos { x, y: y - 1 },
            Dir::DownLeft => RawPos { x: x - 1, y },
            Dir::UpLeft => RawPos { x: x - 1, y: y + 1 },
        }
    }

    fn dir_to(self, other: RawPos) -> Option<(Dir, i32)> {
        match other - self {
            RawPos { x: 0, y } if y > 0 => Some((Dir::Up, y)),
            RawPos { x: 0, y } if y < 0 => Some((Dir::Down, -y)),
            RawPos { x, y: 0 } if x > 0 => Some((Dir::UpRight, x)),
            RawPos { x, y: 0 } if x < 0 => Some((Dir::DownLeft, -x)),
            RawPos { x, y } if x + y == 0 && x > 0 => Some((Dir::DownRight, x)),
            RawPos { x, y } if x + y == 0 && x < 0 => Some((Dir::UpLeft, y)),
            _ => None,
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

    fn min_pw(self, other: RawPos) -> RawPos {
        RawPos {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
        }
    }

    fn max_pw(self, other: RawPos) -> RawPos {
        RawPos {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
        }
    }
}

impl Add for RawPos {
    type Output = Self;

    fn add(self, other: RawPos) -> RawPos {
        RawPos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for RawPos {
    type Output = Self;

    fn sub(self, other: RawPos) -> RawPos {
        RawPos {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Div<i32> for RawPos {
    type Output = Self;

    fn div(self, i: i32) -> RawPos {
        RawPos {
            x: self.x / i,
            y: self.y / i,
        }
    }
}

impl fmt::Display for RawPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl FromStr for RawPos {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s
            .trim()
            .trim_matches(|p| p == '(' || p == ')')
            .split(',')
            .collect();

        let x_fromstr = coords[0].parse::<i32>()?;
        let y_fromstr = coords[1].parse::<i32>()?;

        Ok(RawPos {
            x: x_fromstr,
            y: y_fromstr,
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Pos(pub RawPos);

impl Pos {
    pub(crate) fn from_raw_pos(p: RawPos) -> Pos {
        Pos(p)
    }

    pub(crate) fn to_rel(self, offset: RawPos) -> RelPos {
        RelPos(self.0 - offset)
    }

    fn min_dist(self, b: Pos) -> u32 {
        self.0.min_dist(b.0)
    }

    pub(crate) fn neighbours(self) -> [Pos; 6] {
        let RawPos { x, y } = self.0;
        [
            Pos(RawPos { x, y: y + 1 }),
            Pos(RawPos { x: x + 1, y }),
            Pos(RawPos { x: x + 1, y: y - 1 }),
            Pos(RawPos { x, y: y - 1 }),
            Pos(RawPos { x: x - 1, y }),
            Pos(RawPos { x: x - 1, y: y + 1 }),
        ]
    }

    pub(crate) fn go(self, d: Dir) -> Pos {
        Pos(self.0.go(d))
    }

    pub(crate) fn dir_to(self, other: Pos) -> Option<(Dir, i32)> {
        self.0.dir_to(other.0)
    }

    fn min_pw(self, other: Pos) -> Pos {
        Pos(self.0.min_pw(other.0))
    }

    fn max_pw(self, other: Pos) -> Pos {
        Pos(self.0.max_pw(other.0))
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Pos {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pos = s.parse::<RawPos>()?;
        Ok(Pos(pos))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(crate) struct RelPos(RawPos);

impl RelPos {
    // pub(crate) fn from_pos(p: Pos) -> RelPos {
    //     RelPos(p)
    // }

    pub(crate) fn to_abs(self, offset: RawPos) -> Pos {
        Pos(self.0 + offset)
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
pub(crate) enum Dir {
    Up,
    UpRight,
    DownRight,
    Down,
    DownLeft,
    UpLeft,
}

impl Dir {
    pub(crate) const fn dirs() -> [Dir; 6] {
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
    val: Pos,
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
    data: PosMap<PieceStack>,
    bl: Pos,
    tr: Pos,
}

#[derive(Debug, Clone)]
pub(crate) struct PosMap<T>([[T; BOARD_SIZE]; BOARD_SIZE]);

impl<T: Default> Default for PosMap<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> Index<Pos> for PosMap<T> {
    type Output = T;

    fn index(&self, index: Pos) -> &Self::Output {
        self.get(index)
    }
}

impl<T> IndexMut<Pos> for PosMap<T> {
    fn index_mut(&mut self, index: Pos) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T> PosMap<T> {
    fn get(&self, pos: Pos) -> &T {
        &self.0[((pos.0.x + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
            [((pos.0.y + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
    }

    fn get_mut(&mut self, pos: Pos) -> &mut T {
        &mut self.0[((pos.0.x + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
            [((pos.0.y + BOARD_SIZE as i32 / 2).rem_euclid(BOARD_SIZE as i32)) as usize]
    }

    fn map<U>(self, f: impl Fn(T) -> U) -> PosMap<U> {
        PosMap(self.0.map(|r| r.map(&f)))
    }
}

impl<T: Copy> PosMap<T> {
    fn with_default(val: T) -> Self {
        Self([[val; BOARD_SIZE]; BOARD_SIZE])
    }
}

impl Hive {
    pub(crate) fn new() -> Hive {
        Hive {
            data: Default::default(),
            bl: Pos(RawPos { x: 0, y: 0 }),
            tr: Pos(RawPos { x: 0, y: 0 }),
        }
    }

    pub(crate) fn offset(&self) -> RawPos {
        (self.tr.0 + self.bl.0) / 2
    }

    pub(crate) fn push(&mut self, dst: Pos, piece: Piece) {
        self.bl = self.bl.min_pw(dst);
        self.tr = self.tr.max_pw(dst);
        self.data[dst].push(piece);
    }

    pub(crate) fn pop(&mut self, src: Pos) -> Option<Piece> {
        let s = &mut self.data[src];
        if let Some(piece) = s.pop() {
            if s.is_empty() {
                if src.0.x == self.bl.0.x {
                    let mut found = false;
                    // Look for other pieces with the same x coordinate.
                    for y in self.bl.0.y..=self.tr.0.y {
                        if !self.is_free(Pos(RawPos { x: src.0.x, y })) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 larger.
                        // We need only check the neighbours of src,
                        // as if there are none then src must have contained the last piece.
                        for y in [src.0.y - 1, src.0.y] {
                            if !self.is_free(Pos(RawPos { x: src.0.x + 1, y })) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            self.bl.0.x += 1;
                        } else {
                            // There are no pieces left.
                            self.bl.0.x = 0;
                        }
                    }
                }
                if src.0.y == self.bl.0.y {
                    let mut found = false;
                    // Look for other pieces with the same y coordinate.
                    for x in self.bl.0.x..=self.tr.0.x {
                        if !self.is_free(Pos(RawPos { x, y: src.0.y })) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 larger.
                        // We need only check the neighbours of src,
                        // as if there are none then src must have contained the last piece.
                        for x in [src.0.x - 1, src.0.x] {
                            if !self.is_free(Pos(RawPos { x, y: src.0.y + 1 })) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            self.bl.0.y += 1;
                        } else {
                            // There are no pieces left.
                            self.bl.0.y = 0;
                        }
                    }
                }
                if src.0.x == self.tr.0.x {
                    let mut found = false;
                    // Look for other pieces with the same x coordinate.
                    for y in self.bl.0.y..=self.tr.0.y {
                        if !self.is_free(Pos(RawPos { x: src.0.x, y })) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 smaller.
                        // We need only check the neighbours of src,
                        // as if there are none then src must have contained the last piece.
                        for y in [src.0.y, src.0.y + 1] {
                            if !self.is_free(Pos(RawPos { x: src.0.x - 1, y })) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            self.tr.0.x -= 1;
                        } else {
                            // There are no pieces left.
                            self.tr.0.x = 0;
                        }
                    }
                }
                if src.0.y == self.tr.0.y {
                    let mut found = false;
                    // Look for other pieces with the same y coordinate.
                    for x in self.bl.0.x..=self.tr.0.x {
                        if !self.is_free(Pos(RawPos { x, y: src.0.y })) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 smaller.
                        // We need only check the neighbours of src,
                        // as if there are none then src must have contained the last piece.
                        for x in [src.0.x, src.0.x + 1] {
                            if !self.is_free(Pos(RawPos { x, y: src.0.y - 1 })) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            self.tr.0.y -= 1;
                        } else {
                            // There are no pieces left.
                            self.tr.0.y = 0;
                        }
                    }
                }
            }
            return Some(piece);
        }
        None
    }

    pub fn occupied(&self) -> impl Iterator<Item = Pos> + '_ {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| Pos(RawPos { x, y })))
            .filter(move |p| !self.is_free(*p))
    }

    pub fn bounds(&self) -> (Pos, Pos) {
        (self.bl, self.tr)
    }

    pub fn tiles(&self) -> impl Iterator<Item = (Pos, Piece)> + '_ {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| Pos(RawPos { x, y })))
            .filter_map(move |p| self[p].last().map(|&t| (p, t)))
    }

    pub(crate) fn all(&self) -> impl Iterator<Item = (Pos, usize, &Piece)> {
        (self.bl.0.x..=self.tr.0.x)
            .flat_map(move |x| (self.bl.0.y..=self.tr.0.y).map(move |y| Pos(RawPos { x, y })))
            .flat_map(move |p| self[p].iter().enumerate().map(move |(h, v)| (p, h, v)))
    }

    pub(crate) fn is_free(&self, p: Pos) -> bool {
        self[p].is_empty()
    }

    fn count(&self, p: Pos) -> usize {
        self[p].len()
    }

    pub(crate) fn neighbours(&self, p: Pos, exclude: Option<Pos>) -> impl Iterator<Item = &Piece> {
        p.neighbours().into_iter().filter_map(move |p| {
            if let Some(e) = exclude {
                if p == e {
                    return None;
                }
            }
            self[p].last()
        })
    }

    fn are_connected(&self, a: Pos, b: Pos, excluding: Pos) -> bool {
        // Performs an A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists = PosMap::with_default(u32::MAX);
        dists[a] = 0;
        heap.push(CostItem {
            cost: a.min_dist(b),
            val: a,
        });

        while let Some(CostItem { cost, val }) = heap.pop() {
            if val == b {
                return true;
            }

            if cost > dists[val] + val.min_dist(b) {
                // A better path to this node has already been found
                continue;
            }

            for p in val
                .neighbours()
                .into_iter()
                .filter(|&p| !self.is_free(p) && p != excluding)
            {
                let dist = dists[val] + 1;
                if dist < dists[p] {
                    heap.push(CostItem {
                        cost: dist + p.min_dist(b),
                        val: p,
                    });
                    dists[p] = dist;
                }
            }
        }
        false
    }

    pub(crate) fn is_structural(&self, p: Pos) -> bool {
        if self.count(p) > 1 {
            return false;
        }
        // find representatives of each group of neighbours.
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
        false
    }

    fn queen_dests(&self, p: Pos) -> Vec<Pos> {
        let ns = p.neighbours();
        let surrounding = ns.map(|p| self.is_free(p));
        let mut out = Vec::new();
        for d in 0..6 {
            if surrounding[d]
                && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                && self.neighbours(ns[d], Some(p)).next().is_some()
            {
                out.push(ns[d]);
            }
        }
        out
    }

    fn may_move_queen(&self, src: Pos, dst: Pos) -> bool {
        let ns = src.neighbours();
        let surrounding = ns.map(|p| self.is_free(p));
        let d = match ns.into_iter().position(|p| p == dst) {
            Some(d) => d,
            None => return false,
        };
        return surrounding[d]
            && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
            && self.neighbours(ns[d], Some(src)).next().is_some();
    }

    fn beetle_dests(&self, p: Pos) -> Vec<Pos> {
        let ns = p.neighbours();
        let surrounding = ns.map(|p| self.count(p));
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
        out
    }

    fn may_move_beetle(&self, src: Pos, dst: Pos) -> bool {
        let ns = src.neighbours();
        let surrounding = ns.map(|src| self.count(src));
        let src_height = self.count(src) - 1;
        let d = match ns.into_iter().position(|src| src == dst) {
            Some(d) => d,
            None => return false,
        };
        return surrounding[d].max(src_height)
            >= surrounding[(d + 5) % 6].min(surrounding[(d + 1) % 6])
            && (src_height > 0 || self.neighbours(ns[d], Some(src)).next().is_some());
    }

    fn hopper_dests(&self, p: Pos) -> Vec<Pos> {
        Dir::dirs()
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
                Some(pos)
            })
            .collect()
    }

    fn may_move_hopper(&self, src: Pos, dst: Pos) -> bool {
        let (dir, dist) = match src.dir_to(dst) {
            Some(vs @ (_, dist)) if dist > 1 => vs,
            _ => return false,
        };

        let mut pos = src.go(dir);
        for _ in 1..dist {
            if self.is_free(pos) {
                return false;
            }
            pos = pos.go(dir);
        }

        self.is_free(pos)
    }

    fn ant_dests(&self, src: Pos) -> Vec<Pos> {
        // TODO optimize / cache
        let mut visited = AHashSet::new();
        let mut to_consider = VecDeque::new();
        visited.insert(src);
        to_consider.push_back(src);
        while let Some(pos) = to_consider.pop_front() {
            let ns = pos.neighbours();
            let surrounding = ns.map(|p| self.is_free(p) || p == src);
            for d in 0..6 {
                if !visited.contains(&ns[d])
                    && surrounding[d]
                    && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                    && self.neighbours(ns[d], Some(src)).next().is_some()
                {
                    visited.insert(ns[d]);
                    to_consider.push_back(ns[d]);
                }
            }
        }
        visited.into_iter().filter(|&pos| pos != src).collect()
    }

    fn may_move_ant(&self, src: Pos, dst: Pos) -> bool {
        // Performs an A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists = PosMap::with_default(u32::MAX);
        dists[src] = 0;
        heap.push(CostItem {
            cost: src.min_dist(dst),
            val: src,
        });

        while let Some(CostItem { cost, val }) = heap.pop() {
            if val == dst {
                return true;
            }

            if cost > dists[val] + val.min_dist(dst) {
                // A better path to this node has already been found
                continue;
            }

            let ns = val.neighbours();
            let surrounding = ns.map(|p| self.is_free(p) || p == src);
            for d in 0..6 {
                if surrounding[d]
                    && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                    && self.neighbours(ns[d], Some(src)).next().is_some()
                {
                    let p = ns[d];
                    let dist = dists[val] + 1;
                    if dist < dists[p] {
                        heap.push(CostItem {
                            cost: dist + p.min_dist(dst),
                            val: p,
                        });
                        dists[p] = dist;
                    }
                }
            }
        }
        false
    }

    fn spider_dests(&self, p: Pos) -> Vec<Pos> {
        let mut reach_1 = AHashSet::new();
        let mut reach_2 = AHashSet::new();
        let mut reach_3 = AHashSet::new();
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

    fn may_move_spider(&self, src: Pos, dst: Pos) -> bool {
        // Performs a depth-limited A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists = PosMap::with_default(u32::MAX);
        dists[src] = 0;
        heap.push(CostItem {
            cost: src.min_dist(dst),
            val: src,
        });

        while let Some(CostItem { cost, val }) = heap.pop() {
            if val == dst {
                return cost == 3;
            }

            if cost > dists[val] + val.min_dist(dst) {
                // A better path to this node has already been found
                continue;
            }

            let ns = val.neighbours();
            let surrounding = ns.map(|p| self.is_free(p) || p == src);
            for d in 0..6 {
                if surrounding[d] && (surrounding[(d + 5) % 6] != surrounding[(d + 1) % 6]) {
                    let p = ns[d];
                    let dist = dists[val] + 1;
                    if dist < dists[p] {
                        let cost = dist + p.min_dist(dst);
                        if cost <= 3 {
                            heap.push(CostItem { cost, val: p });
                            dists[p] = dist;
                        }
                    }
                }
            }
        }
        false
    }

    pub(crate) fn may_move(&self, typ: PieceType, src: Pos, dst: Pos) -> bool {
        if self.is_structural(src) {
            return false;
        }
        match typ {
            PieceType::Queen => self.may_move_queen(src, dst),
            PieceType::Beetle => self.may_move_beetle(src, dst),
            PieceType::Hopper => self.may_move_hopper(src, dst),
            PieceType::Ant => self.may_move_ant(src, dst),
            PieceType::Spider => self.may_move_spider(src, dst),
        }
    }

    pub(crate) fn may_place(&self, col: Colour, dst: Pos) -> bool {
        self.neighbours(dst, None).any(|_| true)
            && self.neighbours(dst, None).all(|piece| piece.col() == col)
    }

    pub(crate) fn destinations(&self, p: Pos, typ: PieceType) -> Vec<Pos> {
        match typ {
            PieceType::Queen => self.queen_dests(p),
            PieceType::Beetle => self.beetle_dests(p),
            PieceType::Hopper => self.hopper_dests(p),
            PieceType::Ant => self.ant_dests(p),
            PieceType::Spider => self.spider_dests(p),
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

impl Index<Pos> for Hive {
    type Output = [Piece];

    fn index(&self, index: Pos) -> &Self::Output {
        &self.data[index]
    }
}
