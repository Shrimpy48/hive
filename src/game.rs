use crate::hive::*;
use crate::render::*;
use enum_map::EnumMap;
use rand::random;
use std::collections::HashMap;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug)]
struct Hand {
    data: EnumMap<PieceType, u8>,
}

impl Hand {
    fn new() -> Hand {
        Hand {
            data: vec![
                (PieceType::Queen, 1),
                (PieceType::Beetle, 2),
                (PieceType::Hopper, 3),
                (PieceType::Spider, 2),
                (PieceType::Ant, 3),
                // (PieceType::Ladybug, 1),
                // (PieceType::Mosquito, 1),
                // (PieceType::Pillbug, 1),
            ]
            .into_iter()
            .collect(),
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

    fn pieces(&self) -> impl Iterator<Item = PieceType> + '_ {
        self.data.iter().filter_map(|(p, &n)| (n > 0).then_some(p))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbsMove {
    Place { piece: PieceType, dst: AbsPos },
    Move { src: AbsPos, dst: AbsPos },
    Skip,
}

impl fmt::Display for AbsMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbsMove::Place { piece, dst } => write!(f, "{} -> {}", piece, dst),
            AbsMove::Move { src, dst } => write!(f, "{} -> {}", src, dst),
            AbsMove::Skip => write!(f, "-"),
        }
    }
}

impl FromStr for AbsMove {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split("->").collect();

        if parts.len() != 2 {
            return Ok(AbsMove::Skip);
        }

        let dst = parts[1].parse::<AbsPos>()?;

        if let Ok(piece) = parts[0].parse::<PieceType>() {
            return Ok(AbsMove::Place { piece, dst });
        }

        let src = parts[0].parse::<AbsPos>()?;

        Ok(AbsMove::Move { src, dst })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelMove {
    Place { piece: PieceType, dst: RelPos },
    Move { src: RelPos, dst: RelPos },
    Skip,
}

pub enum Outcome {
    Win(Colour),
    Draw,
    Ongoing,
}

struct ZobristTable {
    data: HashMap<(RelPos, usize, Piece), u64>,
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

    fn get(&mut self, pos: RelPos, h: usize, piece: Piece) -> u64 {
        *self.data.entry((pos, h, piece)).or_insert_with(random)
    }
}

pub struct Game {
    pub hive: Hive,
    white_hand: Hand,
    black_hand: Hand,
    pub turn: Colour,
    turn_counter: u32,
    pub white_queen: Option<AbsPos>,
    pub black_queen: Option<AbsPos>,
    ztable: ZobristTable,
    reptable: HashMap<u64, u8>,
    current_reps: u8,
}

impl Game {
    pub fn offset_move(&self, m: RelMove) -> AbsMove {
        let offset = self.hive.offset();
        match m {
            RelMove::Place { piece, dst } => AbsMove::Place {
                piece,
                dst: dst.to_abs(offset),
            },
            RelMove::Move { src, dst } => AbsMove::Move {
                src: src.to_abs(offset),
                dst: dst.to_abs(offset),
            },
            RelMove::Skip => AbsMove::Skip,
        }
    }

    pub fn unoffset_move(&self, m: AbsMove) -> RelMove {
        let offset = self.hive.offset();
        match m {
            AbsMove::Place { piece, dst } => RelMove::Place {
                piece,
                dst: dst.to_rel(offset),
            },
            AbsMove::Move { src, dst } => RelMove::Move {
                src: src.to_rel(offset),
                dst: dst.to_rel(offset),
            },
            AbsMove::Skip => RelMove::Skip,
        }
    }

    pub fn make_move(&mut self, m: AbsMove) {
        match m {
            AbsMove::Place { piece, dst } => {
                self.current_hand_mut().remove(piece);
                self.hive.push(
                    dst,
                    Piece {
                        typ: piece,
                        col: self.turn,
                    },
                );
                if piece == PieceType::Queen {
                    *self.current_queen_mut() = Some(dst);
                }
            }
            AbsMove::Move { src, dst } => {
                let piece = self.hive.pop(src).unwrap();
                if piece.typ == PieceType::Queen {
                    *self.current_queen_mut() = Some(dst);
                }
                self.hive.push(dst, piece);
            }
            AbsMove::Skip => {}
        }
        self.turn = self.turn.next();
        self.turn_counter += 1;
        let key = self.hash_key();
        *self.reptable.entry(key).or_insert(0) += 1;
        self.current_reps = *self.reptable.get(&key).unwrap();
    }

    pub fn unmake_move(&mut self, m: AbsMove) {
        let key = self.hash_key();
        *self.reptable.entry(key).or_insert(0) -= 1;
        self.turn_counter -= 1;
        self.turn = self.turn.next();
        match m {
            AbsMove::Place { piece, dst } => {
                self.hive.pop(dst).unwrap();
                if piece == PieceType::Queen {
                    *self.current_queen_mut() = None;
                }
                self.current_hand_mut().add(piece);
            }
            AbsMove::Move { src, dst } => {
                let piece = self.hive.pop(dst).unwrap();
                if piece.typ == PieceType::Queen {
                    *self.current_queen_mut() = Some(src);
                }
                self.hive.push(src, piece);
            }
            AbsMove::Skip => {}
        }
        let key = self.hash_key();
        self.current_reps = *self.reptable.get(&key).unwrap();
    }

    pub fn moves(&self) -> Vec<RelMove> {
        // Special cases:
        // ply 0:
        // 	place (0,0)
        // ply 1:
        // 	place (0,1)
        // turn 4 with no queen: place queen
        // no queen: no moves

        // Placings: Colour -> {Pos}
        // Initial placings: as usual
        // When place or move to: remove dest, check neighbours - add new same-colour, remove blocked other-colour
        // When unplace or move from: add src if legal, check neighbours - remove supported same-colour, add blocked other-colour

        // Connectivity
        // 	Connected undirected graph, find vertex cuts of cardinality 1
        // 	Like a dynamic connectivity problem (but with changing vertices instead of edges)
        //
        // 	A vertex is structural iff it has multiple neighbour groups and is not in a cycle
        //
        // 	If a vertex has multiple neighbour groups, at least 1 of which is a structural separated neighbour, then it is also structural
        // 	Note that A is a separated neighbour of B iff B is a separated neighbour of A

        // Moves
        // Initial moves: as usual
        // When place or move to: add moves from dest, remove moves to dest, recheck neighbours, recheck ants, spiders, grasshoppers
        // When unplace or move from: remove moves from dest, recheck neighbours, recheck ants, spiders, grasshoppers
        // Note that moving an ant does not change its destinations and any piece can return to its previous pos
        if self.over() {
            return Vec::new();
        }
        let offset = self.hive.offset();
        let placeable = if self.current_queen().is_none() && self.turn_counter >= 6 {
            vec![PieceType::Queen]
        } else {
            self.current_hand().pieces().collect()
        };
        let places = if self.turn_counter == 0 {
            vec![AbsPos::from_pos(Pos { x: 0, y: 0 })]
        } else if self.turn_counter == 1 {
            // neighbours(Pos { x: 0, y: 0 }).to_vec()
            vec![AbsPos::from_pos(Pos { x: 0, y: 1 })]
        } else {
            self.hive
                .occupied()
                .flat_map(|p| p.neighbours().into_iter())
                .filter(|&p| self.hive.is_free(p))
                .filter(|&p| {
                    self.hive
                        .neighbours(p, None)
                        .all(|Piece { typ: _, col }| *col == self.turn)
                })
                .collect()
        };
        let placings = places.into_iter().flat_map(|l| {
            placeable.iter().map(move |p| RelMove::Place {
                piece: *p,
                dst: l.to_rel(offset),
            })
        });
        let mut moves = if self.current_queen().is_none() {
            vec![]
        } else {
            self.hive
                .tiles()
                .filter_map(|(p, &Piece { typ, col })| {
                    if col == self.turn {
                        Some((p, typ))
                    } else {
                        None
                    }
                })
                .flat_map(|(p, typ)| {
                    self.hive
                        .destinations(p, typ)
                        .into_iter()
                        .map(move |d| RelMove::Move {
                            src: p.to_rel(offset),
                            dst: d.to_rel(offset),
                        })
                })
                .collect()
        };
        moves.extend(placings);
        if moves.is_empty() {
            moves.push(RelMove::Skip);
        }
        moves
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
            reptable: HashMap::new(),
            current_reps: 1,
        };
        let key = game.hash_key();
        let mut reptable = HashMap::new();
        reptable.insert(key, 1);
        game.reptable = reptable;
        game
    }

    fn current_hand_mut(&mut self) -> &mut Hand {
        match self.turn {
            Colour::White => &mut self.white_hand,
            Colour::Black => &mut self.black_hand,
        }
    }

    fn current_queen_mut(&mut self) -> &mut Option<AbsPos> {
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

    fn current_queen(&self) -> &Option<AbsPos> {
        match self.turn {
            Colour::White => &self.white_queen,
            Colour::Black => &self.black_queen,
        }
    }

    fn hash_key(&mut self) -> u64 {
        let offset = self.hive.offset();
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
        }] ^ self.hive.all().fold(0, |x, (pos, h, piece)| {
            x ^ table.get(pos.to_rel(offset), h, *piece)
        })
    }

    pub fn table_key(&mut self) -> u64 {
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
