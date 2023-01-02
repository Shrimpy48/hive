use crate::render::*;
use crate::small_arrayvec::SmallArrayVec;

use ahash::AHashSet;
use enum_map::Enum;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use std::cmp::Ordering;
use std::collections::{BinaryHeap, VecDeque};
use std::fmt;
use std::num::ParseIntError;
use std::ops::{Add, Index, IndexMut, Sub};
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Default)]
pub struct WrapPos(Pos);

impl WrapPos {
    pub(crate) fn neighbours(self) -> [WrapPos; 6] {
        Dir::dirs().map(|d| self.go(d))
    }

    pub(crate) fn go(self, d: Dir) -> Self {
        match d {
            Dir::Up => Self(Pos {
                x: self.0.x,
                y: (self.0.y + 1) % BOARD_SIZE,
            }),
            Dir::UpRight => Self(Pos {
                x: (self.0.x + 1) % BOARD_SIZE,
                y: self.0.y,
            }),
            Dir::DownRight => Self(Pos {
                x: (self.0.x + 1) % BOARD_SIZE,
                y: (BOARD_SIZE + self.0.y - 1) % BOARD_SIZE,
            }),
            Dir::Down => Self(Pos {
                x: self.0.x,
                y: (BOARD_SIZE + self.0.y - 1) % BOARD_SIZE,
            }),
            Dir::DownLeft => Self(Pos {
                x: (BOARD_SIZE + self.0.x - 1) % BOARD_SIZE,
                y: self.0.y,
            }),
            Dir::UpLeft => Self(Pos {
                x: (BOARD_SIZE + self.0.x - 1) % BOARD_SIZE,
                y: (self.0.y + 1) % BOARD_SIZE,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

impl Pos {
    pub fn new(x: u8, y: u8) -> Self {
        assert!(x < BOARD_SIZE);
        assert!(y < BOARD_SIZE);
        Self { x, y }
    }

    pub(crate) fn neighbours(self) -> [Pos; 6] {
        Dir::dirs().map(|d| self.go(d))
    }

    pub(crate) fn go(self, d: Dir) -> Self {
        match d {
            Dir::Up => Self::new(self.x, self.y + 1),
            Dir::UpRight => Self::new(self.x + 1, self.y),
            Dir::DownRight => Self::new(self.x + 1, self.y - 1),
            Dir::Down => Self::new(self.x, self.y - 1),
            Dir::DownLeft => Self::new(self.x - 1, self.y),
            Dir::UpLeft => Self::new(self.x - 1, self.y + 1),
        }
    }

    fn dir_to(self, other: Pos) -> Option<(Dir, u8)> {
        if self.x == other.x {
            match other.y.cmp(&self.y) {
                Ordering::Greater => Some((Dir::Up, other.y - self.y)),
                Ordering::Less => Some((Dir::Down, self.y - other.y)),
                Ordering::Equal => None,
            }
        } else if self.y == other.y {
            match other.x.cmp(&self.x) {
                Ordering::Greater => Some((Dir::UpRight, other.x - self.x)),
                Ordering::Less => Some((Dir::DownLeft, self.x - other.x)),
                Ordering::Equal => None,
            }
        } else if other.x > self.x && other.y < self.y {
            if other.x - self.x == self.y - other.y {
                Some((Dir::DownRight, other.x - self.x))
            } else {
                None
            }
        } else if other.x < self.x && other.y > self.y {
            if self.x - other.x == other.y - self.y {
                Some((Dir::UpLeft, other.y - self.y))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn min_dist(self, b: Pos) -> u8 {
        let Pos { x: ax, y: ay } = self;
        let neg_az = ax + ay;
        let Pos { x: bx, y: by } = b;
        let neg_bz = bx + by;
        let dx = ax.abs_diff(bx);
        let dy = ay.abs_diff(by);
        let dz = neg_az.abs_diff(neg_bz);
        dx.max(dy.max(dz))
    }

    pub(crate) fn to_abs(self, offset: WrapPos) -> WrapPos {
        WrapPos(self) + offset
    }

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

impl Default for Pos {
    fn default() -> Self {
        Self {
            x: BOARD_SIZE / 2,
            y: BOARD_SIZE / 2,
        }
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

        let x_fromstr = coords[0].parse::<u8>()?;
        let y_fromstr = coords[1].parse::<u8>()?;

        Ok(Pos::new(
            x_fromstr,
            y_fromstr,
        ))
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl WrapPos {
    pub(crate) fn to_rel(self, offset: WrapPos) -> Pos {
        (self - offset).0
    }
}

impl Add for WrapPos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        // Self(Pos {
        //     x: (self.0.x + rhs.0.x) % BOARD_SIZE,
        //     y: (self.0.y + rhs.0.y) % BOARD_SIZE,
        // })
        let mut x = self.0.x + rhs.0.x;
        if x >= BOARD_SIZE {
            x -= BOARD_SIZE;
        }
        let mut y = self.0.y + rhs.0.y;
        if y >= BOARD_SIZE {
            y -= BOARD_SIZE;
        }
        Self(Pos { x, y })
    }
}

impl Sub for WrapPos {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        // Self(Pos {
        //     x: (BOARD_SIZE + self.0.x - rhs.0.x) % BOARD_SIZE,
        //     y: (BOARD_SIZE + self.0.y - rhs.0.y) % BOARD_SIZE,
        // })
        let x = if self.0.x >= rhs.0.x {
            self.0.x - rhs.0.x
        } else {
            BOARD_SIZE - rhs.0.x + self.0.x
        };
        let y = if self.0.y >= rhs.0.y {
            self.0.y - rhs.0.y
        } else {
            BOARD_SIZE - rhs.0.y + self.0.y
        };
        Self(Pos { x, y })
    }
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

    fn to_vec(self) -> WrapPos {
        match self {
            Dir::Up => WrapPos(Pos { x: 0, y: 1 }),
            Dir::UpRight => WrapPos(Pos { x: 1, y: 0 }),
            Dir::DownRight => WrapPos(Pos {
                x: 1,
                y: BOARD_SIZE - 1,
            }),
            Dir::Down => WrapPos(Pos {
                x: 0,
                y: BOARD_SIZE - 1,
            }),
            Dir::DownLeft => WrapPos(Pos {
                x: BOARD_SIZE - 1,
                y: 0,
            }),
            Dir::UpLeft => WrapPos(Pos {
                x: BOARD_SIZE - 1,
                y: 1,
            }),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct CostItem {
    cost: u8,
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
// The board must have space to put all of these in a line with a 2 piece gap at the end,
// so that pieces cannot jump from one end to the other.
// Searching for ant moves requires a 2 piece gap on each side at the moment.
const BOARD_SIZE: u8 = 32;
// const BOARD_SIZE: u8 = 28;

// Uses a fixed-size toroidal playing surface.
// The hive can move arbitrarily far, but its size is bounded by the number of pieces,
// so this representation correctly models the hive.
// The origin is placed in the middle of the board so that wrapping occurs infrequently
// as this should benefit caching.
#[derive(Debug, Clone, Default)]
pub struct Hive {
    data: WrapPosMap<PieceStack>,
    bl: WrapPos,
    tr: WrapPos,
    // An arbitrary point in the hive for searching from.
    pos_in_hive: WrapPos,
}

#[derive(Debug, Clone)]
pub(crate) struct PosMap<T>([[T; BOARD_SIZE as usize]; BOARD_SIZE as usize]);

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
        &self.0[pos.x as usize][pos.y as usize]
    }

    fn get_mut(&mut self, pos: Pos) -> &mut T {
        &mut self.0[pos.x as usize][pos.y as usize]
    }

    fn map<U>(self, f: impl Fn(T) -> U) -> PosMap<U> {
        PosMap(self.0.map(|r| r.map(&f)))
    }
}

impl<T: Copy> PosMap<T> {
    fn with_default(val: T) -> Self {
        Self([[val; BOARD_SIZE as usize]; BOARD_SIZE as usize])
    }
}

#[derive(Debug, Clone)]
struct WrapPosMap<T>(PosMap<T>);

impl<T: Default> Default for WrapPosMap<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> Index<WrapPos> for WrapPosMap<T> {
    type Output = T;

    fn index(&self, index: WrapPos) -> &Self::Output {
        self.get(index)
    }
}

impl<T> IndexMut<WrapPos> for WrapPosMap<T> {
    fn index_mut(&mut self, index: WrapPos) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T> WrapPosMap<T> {
    fn get(&self, pos: WrapPos) -> &T {
        self.0.get(pos.0)
    }

    fn get_mut(&mut self, pos: WrapPos) -> &mut T {
        self.0.get_mut(pos.0)
    }

    fn map<U>(self, f: impl Fn(T) -> U) -> WrapPosMap<U> {
        WrapPosMap(self.0.map(f))
    }
}

impl<T: Copy> WrapPosMap<T> {
    fn with_default(val: T) -> Self {
        Self(PosMap::with_default(val))
    }
}

impl Hive {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn offset(&self) -> WrapPos {
        let WrapPos(Pos { x: dx, y: dy }) = self.tr - self.bl;
        self.bl
            + WrapPos(Pos {
                x: dx / 2,
                y: dy / 2,
            })
            - WrapPos::default()
    }

    pub(crate) fn push(&mut self, dst: WrapPos, piece: Piece) -> Option<()> {
        // Using relative coordinates allows for ordering.
        let offset = self.offset();
        let dst_rel = dst.to_rel(offset);
        self.bl = self.bl.to_rel(offset).min_pw(dst_rel).to_abs(offset);
        self.tr = self.tr.to_rel(offset).max_pw(dst_rel).to_abs(offset);
        self.data[dst].push(piece)
    }

    pub(crate) fn pop(&mut self, src: WrapPos) -> Option<Piece> {
        // Using relative coordinates allows for ordering.
        let offset = self.offset();
        let src_rel = src.to_rel(offset);
        let s = &mut self.data[src];
        if let Some(piece) = s.pop() {
            if s.is_empty() {
                if self.pos_in_hive == src {
                    let mut found = false;
                    for p in src.neighbours() {
                        if !self.is_free(p) {
                            self.pos_in_hive = p;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        self.pos_in_hive = WrapPos::default();
                    }
                }
                let bl = self.bl.to_rel(offset);
                let tr = self.tr.to_rel(offset);
                let mut new_bl = bl;
                let mut new_tr = tr;
                if src_rel.x == bl.x {
                    let mut found = false;
                    // Look for other pieces with the same x coordinate.
                    for y in bl.y..=tr.y {
                        if !self.is_free_rel(Pos { x: src_rel.x, y }) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 larger.
                        // We need only check the neighbours of src_rel,
                        // as if there are none then src_rel must have contained the last piece.
                        for y in [src_rel.y - 1, src_rel.y] {
                            if !self.is_free_rel(Pos {
                                x: src_rel.x + 1,
                                y,
                            }) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            new_bl.x += 1;
                        } else {
                            // There are no pieces left.
                            new_bl.x = Pos::default().x;
                        }
                    }
                }
                if src_rel.y == bl.y {
                    let mut found = false;
                    // Look for other pieces with the same y coordinate.
                    for x in bl.x..=tr.x {
                        if !self.is_free_rel(Pos { x, y: src_rel.y }) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 larger.
                        // We need only check the neighbours of src_rel,
                        // as if there are none then src_rel must have contained the last piece.
                        for x in [src_rel.x - 1, src_rel.x] {
                            if !self.is_free_rel(Pos {
                                x,
                                y: src_rel.y + 1,
                            }) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            new_bl.y += 1;
                        } else {
                            // There are no pieces left.
                            new_bl.y = Pos::default().y;
                        }
                    }
                }
                if src_rel.x == tr.x {
                    let mut found = false;
                    // Look for other pieces with the same x coordinate.
                    for y in bl.y..=tr.y {
                        if !self.is_free_rel(Pos { x: src_rel.x, y }) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 smaller.
                        // We need only check the neighbours of src_rel,
                        // as if there are none then src_rel must have contained the last piece.
                        for y in [src_rel.y, src_rel.y + 1] {
                            if !self.is_free_rel(Pos {
                                x: src_rel.x - 1,
                                y,
                            }) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            new_tr.x -= 1;
                        } else {
                            // There are no pieces left.
                            new_tr.x = Pos::default().x;
                        }
                    }
                }
                if src_rel.y == tr.y {
                    let mut found = false;
                    // Look for other pieces with the same y coordinate.
                    for x in bl.x..=tr.x {
                        if !self.is_free_rel(Pos { x, y: src_rel.y }) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Look for other pieces with an x coordinate 1 smaller.
                        // We need only check the neighbours of src_rel,
                        // as if there are none then src_rel must have contained the last piece.
                        for x in [src_rel.x, src_rel.x + 1] {
                            if !self.is_free_rel(Pos {
                                x,
                                y: src_rel.y - 1,
                            }) {
                                found = true;
                                break;
                            }
                        }
                        if found {
                            new_tr.y -= 1;
                        } else {
                            // There are no pieces left.
                            new_tr.y = Pos::default().y;
                        }
                    }
                }
                self.bl = new_bl.to_abs(offset);
                self.tr = new_tr.to_abs(offset);
            }
            return Some(piece);
        }
        None
    }

    pub(crate) fn get(&self, pos: WrapPos) -> &[Piece] {
        &self.data[pos]
    }

    pub fn occupied(&self) -> impl Iterator<Item = Pos> + '_ {
        let offset = self.offset();
        let bl = self.bl.to_rel(offset);
        let tr = self.tr.to_rel(offset);
        (bl.x..=(tr.x))
            .flat_map(move |x| (bl.y..=(tr.y)).map(move |y| Pos { x, y }))
            .filter(move |p| !self.is_free_rel(*p))
    }

    pub fn bounds(&self) -> (Pos, Pos) {
        let offset = self.offset();
        let bl = self.bl.to_rel(offset);
        let tr = self.tr.to_rel(offset);
        (bl, tr)
    }

    pub fn tiles(&self) -> impl Iterator<Item = (Pos, Piece)> + '_ {
        let offset = self.offset();
        let bl = self.bl.to_rel(offset);
        let tr = self.tr.to_rel(offset);
        (bl.x..=(tr.x))
            .flat_map(move |x| (bl.y..=(tr.y)).map(move |y| Pos { x, y }))
            .filter_map(move |p| self[p].last().map(|&t| (p, t)))
    }

    pub(crate) fn all(&self) -> impl Iterator<Item = (Pos, u8, &Piece)> {
        let offset = self.offset();
        let bl = self.bl.to_rel(offset);
        let tr = self.tr.to_rel(offset);
        (bl.x..=(tr.x))
            .flat_map(move |x| (bl.y..=(tr.y)).map(move |y| Pos { x, y }))
            .flat_map(move |p| {
                self[p]
                    .iter()
                    .enumerate()
                    .map(move |(h, v)| (p, h as u8, v))
            })
    }

    pub(crate) fn is_free(&self, p: WrapPos) -> bool {
        self.get(p).is_empty()
    }

    pub(crate) fn is_free_rel(&self, p: Pos) -> bool {
        self[p].is_empty()
    }

    fn count(&self, p: Pos) -> usize {
        self[p].len()
    }

    pub(crate) fn neighbours(
        &self,
        p: WrapPos,
        exclude: Option<WrapPos>,
    ) -> impl Iterator<Item = &Piece> {
        p.neighbours().into_iter().filter_map(move |p| {
            if let Some(e) = exclude {
                if p == e {
                    return None;
                }
            }
            self.get(p).last()
        })
    }

    pub(crate) fn neighbours_rel(
        &self,
        p: Pos,
        exclude: Option<Pos>,
    ) -> impl Iterator<Item = &Piece> {
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
        let mut dists = PosMap::with_default(u8::MAX);
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
                .filter(|&p| !self.is_free_rel(p) && p != excluding)
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
        ns.dedup_by_key(|n| self.is_free_rel(*n));
        if self.is_free_rel(*ns.first().unwrap()) == self.is_free_rel(*ns.last().unwrap()) {
            ns.pop();
        }
        ns.retain(|n| !self.is_free_rel(*n));

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

    pub(crate) fn is_structural_abs(&self, p: WrapPos) -> bool {
        self.is_structural(p.to_rel(self.offset()))
    }

    pub(crate) fn find_structural(&self) -> PosMap<bool> {
        let mut artic_points = PosMap::default();

        let start = self.pos_in_hive.to_rel(self.offset());

        if self.is_free_rel(start) {
            // The hive is empty.
            return artic_points;
        }

        self.find_structural_impl(
            start,
            None,
            0,
            &mut PosMap::default(),
            &mut PosMap::default(),
            &mut PosMap::default(),
            &mut artic_points,
        );

        artic_points
    }

    // Hopcroft and Tarjan's DFS algorithm for finding articulation points.
    fn find_structural_impl(
        &self,
        pos: Pos,
        parent: Option<Pos>,
        d: u8,
        depth: &mut PosMap<u8>,
        low: &mut PosMap<u8>,
        visited: &mut PosMap<bool>,
        artic_points: &mut PosMap<bool>,
    ) {
        visited[pos] = true;
        depth[pos] = d;
        low[pos] = d;
        let mut child_count = 0;
        let mut is_articulation = false;

        for dir in Dir::dirs() {
            let new_pos = pos.go(dir);
            if self.is_free_rel(new_pos) {
                continue;
            }
            if !visited[new_pos] {
                self.find_structural_impl(
                    new_pos,
                    Some(pos),
                    d + 1,
                    depth,
                    low,
                    visited,
                    artic_points,
                );
                child_count += 1;
                if low[new_pos] >= depth[pos] {
                    is_articulation = true;
                }
                low[pos] = low[pos].min(low[new_pos]);
            } else if parent.map_or(true, |p| p != new_pos) {
                low[pos] = low[pos].min(depth[new_pos]);
            }
        }

        if match parent {
            Some(_) => is_articulation,
            None => child_count > 1,
        } {
            artic_points[pos] = true;
        }
    }

    fn queen_dests(&self, p: Pos) -> Vec<Pos> {
        let ns = p.neighbours();
        let surrounding = ns.map(|p| self.is_free_rel(p));
        let mut out = Vec::new();
        for d in 0..6 {
            if surrounding[d]
                && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                && self.neighbours_rel(ns[d], Some(p)).next().is_some()
            {
                debug_assert!(self.may_move_queen(p, ns[d]));
                out.push(ns[d]);
            }
        }
        out
    }

    fn may_move_queen(&self, src: Pos, dst: Pos) -> bool {
        let ns = src.neighbours();
        let surrounding = ns.map(|p| self.is_free_rel(p));
        let d = match ns.into_iter().position(|p| p == dst) {
            Some(d) => d,
            None => return false,
        };
        return surrounding[d]
            && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
            && self.neighbours_rel(ns[d], Some(src)).next().is_some();
    }

    fn beetle_dests(&self, p: Pos) -> Vec<Pos> {
        let ns = p.neighbours();
        let surrounding = ns.map(|p| self.count(p));
        let src_height = self.count(p) - 1;
        let mut out = Vec::new();
        for d in 0..6 {
            if surrounding[d].max(src_height)
                >= surrounding[(d + 5) % 6].min(surrounding[(d + 1) % 6])
                && (src_height > 0 || self.neighbours_rel(ns[d], Some(p)).next().is_some())
            {
                debug_assert!(self.may_move_beetle(p, ns[d]));
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
            && (src_height > 0 || self.neighbours_rel(ns[d], Some(src)).next().is_some());
    }

    fn hopper_dests(&self, p: Pos) -> Vec<Pos> {
        Dir::dirs()
            .into_iter()
            .filter_map(|d| {
                let mut pos = p.go(d);
                if self.is_free_rel(pos) {
                    return None;
                }
                while !self.is_free_rel(pos) {
                    pos = pos.go(d);
                }
                debug_assert!(self.may_move_hopper(p, pos));
                Some(pos)
            })
            .collect()
    }

    fn may_move_hopper(&self, src: Pos, dst: Pos) -> bool {
        if src == dst {
            return false;
        }

        let (dir, dist) = match src.dir_to(dst) {
            Some(vs) => vs,
            _ => return false,
        };

        let mut pos = src.go(dir);
        for _ in 1..dist {
            if self.is_free_rel(pos) {
                return false;
            }
            pos = pos.go(dir);
        }

        self.is_free_rel(pos)
    }

    fn ant_dests(&self, src: Pos) -> Vec<Pos> {
        // TODO: optimize / cache
        let mut visited = AHashSet::new();
        let mut to_consider = VecDeque::new();
        visited.insert(src);
        to_consider.push_back(src);
        while let Some(pos) = to_consider.pop_front() {
            let ns = pos.neighbours();
            let surrounding = ns.map(|p| self.is_free_rel(p) || p == src);
            for d in 0..6 {
                if !visited.contains(&ns[d])
                    && surrounding[d]
                    && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                    && self.neighbours_rel(ns[d], Some(src)).next().is_some()
                {
                    visited.insert(ns[d]);
                    to_consider.push_back(ns[d]);
                }
            }
        }
        visited
            .into_iter()
            .filter(|&pos| pos != src)
            .inspect(|&pos| debug_assert!(self.may_move_ant(src, pos)))
            .collect()
    }

    fn may_move_ant(&self, src: Pos, dst: Pos) -> bool {
        // Performs an A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists = PosMap::with_default(u8::MAX);
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
            let surrounding = ns.map(|p| self.is_free_rel(p) || p == src);
            for d in 0..6 {
                if surrounding[d]
                    && (surrounding[(d + 5) % 6] || surrounding[(d + 1) % 6])
                    && self.neighbours_rel(ns[d], Some(src)).next().is_some()
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
        let surrounding: Vec<bool> = ns.into_iter().map(|pos| self.is_free_rel(pos)).collect();
        for d in 0..6 {
            if surrounding[d] && (surrounding[(d + 5) % 6] != surrounding[(d + 1) % 6]) {
                reach_1.insert(ns[d]);
            }
        }
        for pos in reach_1.iter() {
            let ns = pos.neighbours();
            let surrounding: Vec<bool> = ns
                .iter()
                .map(|&pos| {
                    if pos == p {
                        true
                    } else {
                        self.is_free_rel(pos)
                    }
                })
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
                .map(|&pos| {
                    if pos == p {
                        true
                    } else {
                        self.is_free_rel(pos)
                    }
                })
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
        reach_3
            .into_iter()
            .inspect(|&pos| debug_assert!(self.may_move_spider(p, pos)))
            .collect()
    }

    fn may_move_spider(&self, src: Pos, dst: Pos) -> bool {
        // Performs a depth-limited A* search using min_dist as the heuristic
        let mut heap = BinaryHeap::new();
        let mut dists = PosMap::with_default(u8::MAX);
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
            let surrounding = ns.map(|p| self.is_free_rel(p) || p == src);
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

    pub(crate) fn may_move(&self, typ: PieceType, src: WrapPos, dst: WrapPos) -> bool {
        let offset = self.offset();
        let src_rel = src.to_rel(offset);
        let dst_rel = dst.to_rel(offset);

        if self.is_structural(src_rel) {
            return false;
        }
        match typ {
            PieceType::Queen => self.may_move_queen(src_rel, dst_rel),
            PieceType::Beetle => self.may_move_beetle(src_rel, dst_rel),
            PieceType::Hopper => self.may_move_hopper(src_rel, dst_rel),
            PieceType::Ant => self.may_move_ant(src_rel, dst_rel),
            PieceType::Spider => self.may_move_spider(src_rel, dst_rel),
        }
    }

    pub(crate) fn may_place(&self, col: Colour, dst: WrapPos) -> bool {
        let offset = self.offset();
        let dst_rel = dst.to_rel(offset);

        self.neighbours_rel(dst_rel, None).any(|_| true)
            && self
                .neighbours_rel(dst_rel, None)
                .all(|piece| piece.col() == col)
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
        let mut canvas = Canvas::new();
        let mut to_draw: Vec<_> = self.all().collect();
        to_draw.sort_unstable_by_key(|(_, h, _)| *h);
        for (pos, h, piece) in to_draw {
            let rel_x = pos.x as i16 - Pos::default().x as i16;
            let rel_y = pos.y as i16 - Pos::default().y as i16;
            let x = rel_x * 6 - h as i16;
            let y = -(rel_x * 2 + rel_y * 4) - h as i16;
            piece.put(&mut canvas, (x, y));
        }
        f.write_str(&canvas.render())
    }
}

impl Index<Pos> for Hive {
    type Output = [Piece];

    fn index(&self, index: Pos) -> &Self::Output {
        &self.data[index.to_abs(self.offset())]
    }
}
