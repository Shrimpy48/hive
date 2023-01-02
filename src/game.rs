use crate::hive::*;
use crate::render::*;
use ahash::AHashMap;
use ahash::AHashSet;
use enum_map::{enum_map, EnumMap};
use rand::random;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug)]
pub struct Hand {
    data: EnumMap<PieceType, u8>,
}

impl Hand {
    fn new() -> Hand {
        Hand {
            data: enum_map! {
                PieceType::Queen => 1,
                PieceType::Beetle => 2,
                PieceType::Hopper => 3,
                PieceType::Spider => 2,
                PieceType::Ant => 3,
                // PieceType::Ladybug => 1,
                // PieceType::Mosquito => 1,
                // PieceType::Pillbug => 1,
            },
        }
    }

    fn remove(&mut self, piece: PieceType) -> bool {
        let c = &mut self.data[piece];
        if *c > 0 {
            *c -= 1;
            true
        } else {
            false
        }
    }

    fn add(&mut self, piece: PieceType) {
        self.data[piece] += 1;
    }

    pub fn pieces(&self) -> impl Iterator<Item = PieceType> + '_ {
        self.data.iter().filter_map(|(p, &n)| (n > 0).then_some(p))
    }

    fn count(&self, piece: PieceType) -> u8 {
        self.data[piece]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Move {
    Place { piece: PieceType, dst: Pos },
    Move { src: Pos, dst: Pos },
    Skip,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WrapMove {
    Place { piece: PieceType, dst: WrapPos },
    Move { src: WrapPos, dst: WrapPos },
    Skip,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Move::Place { piece, dst } => write!(f, "{} -> {}", piece, dst),
            Move::Move { src, dst } => write!(f, "{} -> {}", src, dst),
            Move::Skip => write!(f, "-"),
        }
    }
}

impl FromStr for Move {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split("->").collect();

        if parts.len() != 2 {
            return Ok(Move::Skip);
        }

        let dst = parts[1].parse::<Pos>()?;

        if let Ok(piece) = parts[0].parse::<PieceType>() {
            return Ok(Move::Place { piece, dst });
        }

        let src = parts[0].parse::<Pos>()?;

        Ok(Move::Move { src, dst })
    }
}

pub enum Outcome {
    Win(Colour),
    Draw,
    Ongoing,
}

struct ZobristTable {
    data: AHashMap<(Pos, u8, Piece), u64>,
    turn_num: [u64; 9],
    turn: [u64; 2],
    rep_num: [u64; 5],
}

impl ZobristTable {
    fn new() -> ZobristTable {
        ZobristTable {
            data: Default::default(),
            turn_num: random(),
            turn: random(),
            rep_num: random(),
        }
    }

    fn get(&mut self, pos: Pos, h: u8, piece: Piece) -> u64 {
        *self.data.entry((pos, h, piece)).or_insert_with(random)
    }
}

pub struct Game {
    hive: Hive,
    white_hand: Hand,
    black_hand: Hand,
    turn: Colour,
    turn_counter: u32,
    white_queen: Option<WrapPos>,
    black_queen: Option<WrapPos>,
    ztable: ZobristTable,
    reptable: AHashMap<u64, u8>,
    current_reps: u8,
}

impl Game {
    pub fn hive(&self) -> &Hive {
        &self.hive
    }

    pub fn turn(&self) -> Colour {
        self.turn
    }

    pub(crate) fn white_queen(&self) -> Option<WrapPos> {
        self.white_queen
    }

    pub(crate) fn black_queen(&self) -> Option<WrapPos> {
        self.black_queen
    }

    fn is_legal_abs(&self, m: WrapMove) -> bool {
        if self.over() {
            return false;
        }
        match m {
            WrapMove::Place { piece, dst } => {
                if self.current_queen().is_none()
                    && self.turn_counter >= 6
                    && piece != PieceType::Queen
                {
                    return false;
                }
                if self.current_hand().count(piece) == 0 {
                    return false;
                }
                if self.turn_counter == 0 {
                    return dst == WrapPos::default();
                }
                if self.turn_counter == 1 {
                    return WrapPos::default().neighbours().contains(&dst);
                }
                if !self.hive.get(dst).is_empty() {
                    return false;
                }
                self.hive.may_place(self.turn(), dst)
            }
            WrapMove::Move { src, dst } => {
                if let Some(p) = self.hive.get(src).last() {
                    if p.col() == self.turn() {
                        return self.hive.may_move(p.typ(), src, dst);
                    }
                }
                false
            }
            WrapMove::Skip => {
                // TODO optimise
                self.moves() == [Move::Skip]
            }
        }
    }

    pub fn is_legal(&self, m: Move) -> bool {
        self.is_legal_abs(self.wrap_move(m))
    }

    pub fn wrap_move(&self, m: Move) -> WrapMove {
        let offset = self.hive.offset();
        match m {
            Move::Move { src, dst } => WrapMove::Move {
                src: src.to_abs(offset),
                dst: dst.to_abs(offset),
            },
            Move::Place { piece, dst } => WrapMove::Place {
                piece,
                dst: dst.to_abs(offset),
            },
            Move::Skip => WrapMove::Skip,
        }
    }

    pub fn unwrap_move(&self, m: WrapMove) -> Move {
        let offset = self.hive.offset();
        match m {
            WrapMove::Move { src, dst } => Move::Move {
                src: src.to_rel(offset),
                dst: dst.to_rel(offset),
            },
            WrapMove::Place { piece, dst } => Move::Place {
                piece,
                dst: dst.to_rel(offset),
            },
            WrapMove::Skip => Move::Skip,
        }
    }

    // Checking whether a move _was_ valid is more challenging than
    // checking whether it _is_ valid and also less necessary as the
    // usual way to unmake a move is "undoing" a stored valid move,
    // so this only guarantees that unmaking will not panic
    // or violate hive connectivity.
    fn is_unmake_safe(&self, m: WrapMove) -> bool {
        match m {
            WrapMove::Place { piece, dst } => {
                if let [p] = self.hive.get(dst) {
                    if p.typ() == piece && p.col() != self.turn() {
                        return !self.hive.is_structural_abs(dst);
                    }
                }
                false
            }
            WrapMove::Move { src, dst } => {
                if let Some(p) = self.hive.get(dst).last() {
                    if p.col() != self.turn() {
                        // All piece moves are reversible.
                        return self.hive.may_move(p.typ(), dst, src);
                    }
                }
                false
            }
            WrapMove::Skip => true,
        }
    }

    pub fn make_move(&mut self, m: WrapMove) -> Option<()> {
        if self.is_legal_abs(m) {
            self.make_move_impl(m);
            Some(())
        } else {
            None
        }
    }

    pub fn unmake_move(&mut self, m: WrapMove) -> Option<()> {
        if self.is_unmake_safe(m) {
            self.unmake_move_impl(m);
            Some(())
        } else {
            None
        }
    }

    pub fn make_move_unchecked(&mut self, m: WrapMove) {
        debug_assert!(self.is_legal_abs(m));
        self.make_move_impl(m);
    }

    pub fn unmake_move_unchecked(&mut self, m: WrapMove) {
        debug_assert!(self.is_unmake_safe(m));
        self.unmake_move_impl(m);
    }

    fn make_move_impl(&mut self, m: WrapMove) {
        match m {
            WrapMove::Place { piece, dst } => {
                self.current_hand_mut().remove(piece);
                self.hive.push(dst, Piece::new(piece, self.turn));
                if piece == PieceType::Queen {
                    *self.current_queen_mut() = Some(dst);
                }
            }
            WrapMove::Move { src, dst } => {
                let piece = self.hive.pop(src).unwrap();
                self.hive.push(dst, piece).unwrap();
                if piece.typ() == PieceType::Queen {
                    *self.current_queen_mut() = Some(dst);
                }
            }
            WrapMove::Skip => {}
        }
        self.turn = self.turn.next();
        self.turn_counter += 1;
        let key = self.hash_key();
        *self.reptable.entry(key).or_insert(0) += 1;
        self.current_reps = *self.reptable.get(&key).unwrap();
    }

    fn unmake_move_impl(&mut self, m: WrapMove) {
        let key = self.hash_key();
        *self.reptable.get_mut(&key).unwrap() -= 1;
        self.turn_counter -= 1;
        self.turn = self.turn.next();
        match m {
            WrapMove::Place { piece, dst } => {
                self.hive.pop(dst).unwrap();
                if piece == PieceType::Queen {
                    *self.current_queen_mut() = None;
                }
                self.current_hand_mut().add(piece);
            }
            WrapMove::Move { src, dst } => {
                let piece = self.hive.pop(dst).unwrap();
                self.hive.push(src, piece).unwrap();
                if piece.typ() == PieceType::Queen {
                    *self.current_queen_mut() = Some(src);
                }
            }
            WrapMove::Skip => {}
        }
        let key = self.hash_key();
        self.current_reps = *self.reptable.get(&key).unwrap();
    }

    pub fn moves_abs(&self) -> Vec<WrapMove> {
        self.moves()
            .into_iter()
            .map(|m| self.wrap_move(m))
            .collect()
    }

    pub fn moves(&self) -> Vec<Move> {
        // TODO avoid using vecs everywhere and instead make this an iterator.
        //
        // Special cases:
        // ply 0:
        // 	place (0,0)
        // ply 1:
        // 	place (0,1)
        // turn 4 with no queen: place queen
        // no queen: no moves

        // Placings: Colour -> {RelWrapPos}
        // Initial placings: as usual
        // When place or move to: remove dest, check neighbours - add new same-colour, remove blocked other-colour
        // When unplace or move from: add src if legal, check neighbours - remove supported same-colour, add blocked other-colour

        // Connectivity
        // Find all biconnected components. The structural pieces are the articulation vertices.

        // Moves
        // Initial moves: as usual
        // When place or move to: add moves from dest, remove moves to dest, recheck neighbours, recheck ants, spiders, grasshoppers
        // When unplace or move from: remove moves from dest, recheck neighbours, recheck ants, spiders, grasshoppers
        // Note that moving an ant does not change its destinations and any piece can return to its previous pos
        if self.over() {
            return Vec::new();
        }
        let placeable = if self.current_queen().is_none() && self.turn_counter >= 6 {
            vec![PieceType::Queen]
        } else {
            self.current_hand().pieces().collect()
        };
        let places = self.place_locations();
        let placings = places.into_iter().flat_map(|l| {
            placeable
                .iter()
                .map(move |p| Move::Place { piece: *p, dst: l })
        });
        let mut moves = if self.current_queen().is_none() {
            vec![]
        } else {
            let struct_points = self.hive.find_structural();
            self.hive
                .tiles()
                .filter_map(|(p, piece)| {
                    if piece.col() == self.turn {
                        Some((p, piece.typ()))
                    } else {
                        None
                    }
                })
                .flat_map::<Box<dyn Iterator<Item = Move>>, _>(|(p, typ)| {
                    if struct_points[p] && self.hive[p].len() == 1 {
                        Box::new(std::iter::empty())
                    } else {
                        Box::new(
                            self.hive
                                .destinations(p, typ)
                                .into_iter()
                                .map(move |d| Move::Move { src: p, dst: d }),
                        )
                    }
                })
                .collect()
        };
        moves.extend(placings);
        if moves.is_empty() {
            moves.push(Move::Skip);
        }
        moves
    }

    pub fn place_locations(&self) -> Vec<Pos> {
        let out: AHashSet<Pos> = if self.turn_counter == 0 {
            [Pos::default()].into_iter().collect()
        } else if self.turn_counter == 1 {
            Pos::default().neighbours().into_iter().collect()
        } else {
            self.hive
                .occupied()
                .flat_map(|p| p.neighbours().into_iter())
                .filter(|&p| self.hive.is_free_rel(p))
                .filter(|&p| {
                    self.hive
                        .neighbours_rel(p, None)
                        .all(|piece| piece.col() == self.turn)
                })
                .collect()
        };
        out.into_iter().collect()
    }

    pub fn destinations(&self, src: Pos) -> Vec<Pos> {
        if let Some(piece) = self.hive[src].last() {
            if !self.hive.is_structural(src) {
                return self.hive.destinations(src, piece.typ());
            }
        }
        vec![]
    }

    pub fn over(&self) -> bool {
        !matches!(self.result(), Outcome::Ongoing)
    }

    pub fn result(&self) -> Outcome {
        if self.current_reps > 4 {
            return Outcome::Draw;
        }
        match (self.white_queen, self.black_queen) {
            (Some(wp), Some(bp)) => {
                let white_lost = self.hive.neighbours(wp, None).count() == 6;
                let black_lost = self.hive.neighbours(bp, None).count() == 6;
                if white_lost && black_lost {
                    Outcome::Draw
                } else if white_lost {
                    Outcome::Win(Colour::Black)
                } else if black_lost {
                    Outcome::Win(Colour::White)
                } else {
                    Outcome::Ongoing
                }
            }
            _ => Outcome::Ongoing,
        }
    }

    pub fn new() -> Game {
        let mut game = Game {
            hive: Hive::new(),
            white_hand: Hand::new(),
            black_hand: Hand::new(),
            turn: Colour::White,
            turn_counter: 0,
            white_queen: None,
            black_queen: None,
            ztable: ZobristTable::new(),
            reptable: AHashMap::new(),
            current_reps: 1,
        };
        let key = game.hash_key();
        let mut reptable = AHashMap::new();
        reptable.insert(key, 1);
        game.reptable = reptable;
        game
    }

    pub fn white_hand(&self) -> &Hand {
        &self.white_hand
    }

    pub fn black_hand(&self) -> &Hand {
        &self.black_hand
    }

    fn current_hand_mut(&mut self) -> &mut Hand {
        match self.turn {
            Colour::White => &mut self.white_hand,
            Colour::Black => &mut self.black_hand,
        }
    }

    fn current_queen_mut(&mut self) -> &mut Option<WrapPos> {
        match self.turn {
            Colour::White => &mut self.white_queen,
            Colour::Black => &mut self.black_queen,
        }
    }

    fn current_hand(&self) -> &Hand {
        match self.turn {
            Colour::White => &self.white_hand,
            Colour::Black => &self.black_hand,
        }
    }

    pub fn current_queen(&self) -> &Option<WrapPos> {
        match self.turn {
            Colour::White => &self.white_queen,
            Colour::Black => &self.black_queen,
        }
    }

    pub fn turn_counter(&self) -> u32 {
        self.turn_counter
    }

    fn hash_key(&mut self) -> u64 {
        let table = &mut self.ztable;
        (match self.turn {
            Colour::White => table.turn[0],
            Colour::Black => table.turn[1],
        }) ^ table.turn_num[if self.white_queen.is_none()
            || self.black_queen.is_none()
            || self.turn_counter < 2
        {
            self.turn_counter as usize
        } else {
            8
        }] ^ self
            .hive
            .all()
            .fold(0, |x, (pos, h, piece)| x ^ table.get(pos, h, *piece))
    }

    pub(crate) fn table_key(&mut self) -> u64 {
        let rep_hash = self.ztable.rep_num[(self.current_reps - 1) as usize];
        rep_hash ^ self.hash_key()
    }
}

impl fmt::Debug for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Game")
            .field("hive", &self.hive)
            .field("white_hand", &self.white_hand)
            .field("black_hand", &self.black_hand)
            .field("turn", &self.turn)
            .field("turn_counter", &self.turn_counter)
            .field("white_queen", &self.white_queen)
            .field("black_queen", &self.black_queen)
            .finish()
    }
}

impl fmt::Display for Hand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut canvas = Canvas::new();
        let mut x = 0;
        for (piece, count) in self.data.iter() {
            let l1 = match piece {
                PieceType::Queen => ['B', 'E', 'E'],
                PieceType::Beetle => ['B', 'T', 'L'],
                PieceType::Hopper => ['G', 'H', 'R'],
                PieceType::Spider => ['S', 'P', 'I'],
                PieceType::Ant => ['A', 'N', 'T'],
                // PieceType::Ladybug => ['L', 'D', 'Y'],
                // PieceType::Mosquito => ['M', 'O', 'Z'],
                // PieceType::Pillbug => ['P', 'I', 'L'],
            };
            let l2 = match count {
                0 => [' ', '0', ' '],
                1 => [' ', '1', ' '],
                2 => [' ', '2', ' '],
                3 => [' ', '3', ' '],
                4 => [' ', '4', ' '],
                5 => [' ', '5', ' '],
                6 => [' ', '6', ' '],
                7 => [' ', '7', ' '],
                8 => [' ', '8', ' '],
                9 => [' ', '9', ' '],
                i => panic!("Count too large: {}", i),
            };
            canvas.put_tile((x, 0), l1, l2);
            x += 7;
        }
        f.write_str(&canvas.render())
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.black_hand)?;
        match self.turn {
            Colour::White => {
                writeln!(f, "B ---------------------------------")?;
                write!(f, "{}", self.hive)?;
                writeln!(f, "W --------------------------------- *")?;
            }
            Colour::Black => {
                writeln!(f, "B --------------------------------- *")?;
                write!(f, "{}", self.hive)?;
                writeln!(f, "W ---------------------------------")?;
            }
        }
        write!(f, "{}", self.white_hand)
    }
}

impl Default for Game {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use rand::{prelude::SliceRandom, thread_rng};

    use super::*;

    #[test]
    fn move_list_legal() {
        let mut rng = thread_rng();
        'trials: for _ in 0..1000 {
            let mut game = Game::new();
            for _ in 0..100 {
                let moves = game.moves();
                for &m in moves.iter() {
                    if !game.is_legal(m) {
                        eprintln!("move {m} illegal in position\n{game}");
                        eprintln!("hive bounds: {:?}", game.hive.bounds());
                        panic!("illegal move in move list");
                    }
                }
                match moves.choose(&mut rng) {
                    Some(&m) => game.make_move(game.wrap_move(m)).unwrap(),
                    None => break 'trials,
                }
            }
        }
    }
}
