//! The agent.
//!
//! Contains the searching and transposition storing logic.

use crate::game::*;
use crate::hive::{Colour, Piece};
use rand::rngs::ThreadRng;
use rand::{thread_rng, Rng};
use std::cmp::{Ord, Ordering};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::Neg;

/// The value assigned to a search node.
#[derive(Debug, Eq, Clone, Copy)]
enum Value {
    Win,
    Loss,
    Value(i32),
}

impl Neg for Value {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Value::Win => Value::Loss,
            Value::Loss => Value::Win,
            Value::Value(i) => Value::Value(-i),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Win, Value::Win) => true,
            (Value::Loss, Value::Loss) => true,
            (Value::Value(i), Value::Value(j)) => i == j,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    /// For the purposes of comparison [Value::Loss] acts as -inf and [Value::Win] acts as +inf.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Win, Value::Win) => Ordering::Equal,
            (Value::Win, _) => Ordering::Greater,
            (_, Value::Win) => Ordering::Less,
            (Value::Loss, Value::Loss) => Ordering::Equal,
            (Value::Loss, _) => Ordering::Less,
            (_, Value::Loss) => Ordering::Greater,
            (Value::Value(i), Value::Value(j)) => i.cmp(j),
        }
    }
}

/// A node value stored in the transposition table.
///
/// Because of pruning, this node may not have been fully evaluated,
/// So this can be a bound on the actual node value.
#[derive(Debug, Clone, Copy)]
enum TValue {
    Exact(Value),
    UpperBound(Value),
    LowerBound(Value),
}

impl TValue {
    /// Whether the value is exact as opposed to a bound.
    fn is_exact(&self) -> bool {
        matches!(self, TValue::Exact(_))
    }
}

/// An entry in the transposition table.
#[derive(Debug, Clone)]
struct TEntry {
    /// The depth to which the node was searched.
    d: u8,
    /// The value given to the node.
    val: TValue,
    /// The possible moves from this position,
    /// with the best first.
    ///
    /// This enables PVS and saves us recomputing
    /// the possible moves as this is expensive.
    ms: Vec<RelMove>,
}

/// The transposition table.
///
/// This is implemented as a random replacement cache
/// to avoid expensive bookkeeping.
struct TTable {
    /// The entries in the table.
    data: HashMap<u64, TEntry>,
    /// The maximum number of entries to store in the table.
    max_size: usize,
    rng: ThreadRng,
}

impl TTable {
    /// Create a transposition table with the given size.
    fn new(max_size: usize) -> TTable {
        TTable {
            data: HashMap::with_capacity(max_size),
            max_size,
            rng: thread_rng(),
        }
    }

    /// Get the entry for the given [Game] if one exists.
    ///
    /// This takes a mutable reference to the game in order to
    /// calculate the hash of the game state (see [Game::table_key()]),
    /// which may store new bitstrings.
    fn get(&self, game: &mut Game) -> Option<&TEntry> {
        let key = game.table_key();
        self.data.get(&key)
    }

    /// Insert an entry into the table, removing a random entry if
    /// the table is full.
    ///
    /// If there is already an entry for this position,
    /// it will be replaced if the new entry provides better
    /// information (has been searched to a higher depth
    /// or provides an exact value).
    fn insert(&mut self, game: &mut Game, entry: TEntry) {
        let key = game.table_key();
        match self.data.entry(key) {
            Entry::Occupied(mut oe) => {
                let &TEntry { d, val: _, ms: _ } = oe.get();
                if entry.d > d || (entry.d == d && entry.val.is_exact()) {
                    oe.insert(entry);
                }
            }
            Entry::Vacant(ve) => {
                ve.insert(entry);
                if self.data.len() == self.max_size {
                    let i = self.rng.gen_range(0..self.max_size);
                    let k = *self.data.keys().nth(i).unwrap();
                    self.data.remove(&k);
                }
            }
        }
    }
}

/// An agent for Hive.
pub struct Bot {
    table: TTable,
}

impl Bot {
    pub fn new(table_size: usize) -> Bot {
        Bot {
            table: TTable::new(table_size),
        }
    }

    /// Perform an iterative deepening search for the best move
    /// in this position and make it.
    ///
    /// Will stop once a guaranteed win/loss is found
    /// or the given depth limit is reached.
    pub fn make_best_move(&mut self, game: &mut Game, depth: u8) {
        let ms = game.moves();
        if ms.len() == 1 {
            game.make_move(game.offset_move(ms[0]));
            return;
        }
        let mut best = RelMove::Skip;
        for d in 1..=depth {
            let (m, v) = self.search(game, d, Value::Loss, Value::Win);
            best = m;
            match v {
                Value::Win => {
                    // println!("+M{}", (d + 1) / 2);
                    break;
                }
                Value::Loss => {
                    // println!("-M{}", (d + 1) / 2);
                    break;
                }
                Value::Value(0) => {
                    // println!("0");
                }
                Value::Value(x) => {
                    // println!("{:+}", x);
                }
            }
        }
        game.make_move(game.offset_move(best));
    }

    // /// An estimate for the material value of each piece type.
    // fn act_val(piece: PieceType) -> i32 {
    //     match piece {
    //         PieceType::Queen => 1,
    //         PieceType::Beetle => 2,
    //         PieceType::Hopper => 2,
    //         PieceType::Spider => 2,
    //         PieceType::Ant => 4,
    //         // PieceType::Ladybug => 1,
    //         // PieceType::Mosquito => 1,
    //         // PieceType::Pillbug => 1,
    //     }
    // }

    /// Compute a heuristic for the value of the state
    /// from the current player's perspective.
    ///
    /// This is based on the number of pieces around each queen bee.
    fn static_val(&self, game: &Game) -> Value {
        // The value to white.
        let res = match game.result() {
            Outcome::Win(Colour::White) => Value::Win,
            Outcome::Win(Colour::Black) => Value::Loss,
            Outcome::Draw => Value::Value(0),
            Outcome::Ongoing => {
                // The number of white and black pieces around the white queen.
                let (white_wnc, white_bnc) = match game.white_queen {
                    None => (0, 0),
                    Some(pos) => {
                        let (wn, bn): (Vec<&Piece>, Vec<&Piece>) = game
                            .hive
                            .neighbours(pos, None)
                            .partition(|n| n.col() == Colour::White);
                        (wn.len() as i32, bn.len() as i32)
                    }
                };
                // The number of white and black pieces around the black queen.
                let (black_wnc, black_bnc) = match game.black_queen {
                    None => (0, 0),
                    Some(pos) => {
                        let (wn, bn): (Vec<&Piece>, Vec<&Piece>) = game
                            .hive
                            .neighbours(pos, None)
                            .partition(|n| n.col() == Colour::White);
                        (wn.len() as i32, bn.len() as i32)
                    }
                };
                // let (white_mat_vec, black_mat_vec): (Vec<&Piece>, Vec<&Piece>) = game
                //     .hive
                //     .all()
                //     .map(|(_, _, p)| p)
                //     .partition(|p| p.col == Colour::White);
                // let white_mat: i32 = white_mat_vec
                //     .into_iter()
                //     .map(|p| Self::mat_val(p.typ))
                //     .sum();
                // let black_mat: i32 = black_mat_vec
                //     .into_iter()
                //     .map(|p| Self::mat_val(p.typ))
                //     .sum();
                // let (white_pos_vec, black_pos_vec): (Vec<(&Pos, &Piece)>, Vec<(&Pos, &Piece)>) =
                //     game.hive.tiles().partition(|(_, p)| p.col == Colour::White);
                // let white_act: i32 = white_pos_vec
                //     .into_iter()
                //     .map(|(&pos, piece)| {
                //         Self::act_val(piece.typ)
                //             * game.hive.destinations(pos, piece.typ).len() as i32
                //     })
                //     .sum();
                // let black_act: i32 = black_pos_vec
                //     .into_iter()
                //     .map(|(&pos, piece)| {
                //         Self::act_val(piece.typ)
                //             * game.hive.destinations(pos, piece.typ).len() as i32
                //     })
                //     .sum();
                // pieces on board
                // pieces that can/cannot be moved
                // 	where/how many places
                // pieces around queen
                // whether queen can move
                // how enclosed open spaces around the queen are
                // let white_val = 64 * (black_bnc - 1 + 3 * black_wnc) + white_act; // + white_mat;
                let white_val = black_bnc + black_wnc;
                // let black_val = 64 * (white_wnc - 1 + 3 * white_bnc) + black_act; // + black_mat;
                let black_val = white_wnc + white_bnc;
                return Value::Value(white_val - black_val);
            }
        };
        match game.turn {
            Colour::White => res,
            Colour::Black => -res,
        }
    }

    /// Perform a principle variation search to the given depth.
    ///
    /// `a` and `b` are lower and upper pruning bounds respectively.
    ///
    /// Returns the node value and the move which achieves it.
    fn search(
        &mut self,
        game: &mut Game,
        depth: u8,
        mut a: Value,
        mut b: Value,
    ) -> (RelMove, Value) {
        let mut moves;
        let start_a = a;
        // Check if the node has already been searched.
        // If it has, we can return the value
        // or use it as the starting point for our search.
        if let Some(TEntry { d, val, ms }) = self.table.get(game) {
            if *d >= depth {
                let value;
                match val {
                    TValue::Exact(v) => return (ms[0], *v),
                    TValue::LowerBound(v) => {
                        a = a.max(*v);
                        value = v;
                    }
                    TValue::UpperBound(v) => {
                        b = b.min(*v);
                        value = v;
                    }
                }
                if a >= b {
                    return (ms[0], *value);
                }
            }
            // TODO move pvs and normal negamax to separate functions
            moves = ms.clone();

            // Search the principle variation using normal alpha-beta negamax.
            // This will give us a bound against which to check the other moves.
            let pv = moves[0];
            let mut best = 0;
            let m_ = game.offset_move(pv);
            game.make_move(m_);
            let mut value = -self.val(game, depth - 1, -b, -a);
            game.unmake_move(m_);
            a = a.max(value);

            if a < b {
                // Search the remaining moves to see if they exceed the bound.
                for (i, m) in moves[1..].iter().enumerate() {
                    let m_ = game.offset_move(*m);
                    game.make_move(m_);
                    let mut val;
                    if let Value::Value(a_) = a {
                        // Search with a null window at first.
                        // This will quickly check whether this move exceeds the bound.
                        // If it does, we need to perform a full search to establish a new bound.
                        val = -self.val(game, depth - 1, Value::Value(-a_ - 1), -a);
                        if a < val && val < b {
                            val = -self.val(game, depth - 1, -b, -val);
                        }
                    } else {
                        // We have no good bound yet, so we have to do a full search.
                        val = -self.val(game, depth - 1, -b, -a);
                    }
                    game.unmake_move(m_);
                    if value < val {
                        value = val;
                        best = i + 1;
                    }
                    a = a.max(value);
                    if a >= b {
                        break;
                    }
                }
            }

            // Update the transposition table.
            let tval = if value <= start_a {
                TValue::UpperBound(value)
            } else if value >= b {
                TValue::LowerBound(value)
            } else {
                TValue::Exact(value)
            };
            moves.swap(0, best);
            let best_m = moves[0];
            self.table.insert(
                game,
                TEntry {
                    d: depth,
                    val: tval,
                    ms: moves,
                },
            );

            return (best_m, value);
        }

        // We don't have a principle variation yet,
        // so do normal negamax.
        moves = game.moves();
        let mut value = Value::Loss;
        let mut best = 0;
        for (i, m) in moves.iter().enumerate() {
            let m_ = game.offset_move(*m);
            game.make_move(m_);
            let val = -self.val(game, depth - 1, -b, -a);
            game.unmake_move(m_);
            if value < val {
                value = val;
                best = i;
            }
            a = a.max(value);
            if a >= b {
                break;
            }
        }

        // Update the transposition table.
        let tval = if value <= start_a {
            TValue::UpperBound(value)
        } else if value >= b {
            TValue::LowerBound(value)
        } else {
            TValue::Exact(value)
        };
        moves.swap(0, best);
        let best_m = moves[0];
        self.table.insert(
            game,
            TEntry {
                d: depth,
                val: tval,
                ms: moves,
            },
        );

        (best_m, value)
    }

    /// Compute the value of the state, using [Self::static_val] for depth 0 or terminal nodes and [Self::search] otherwise.
    fn val(&mut self, game: &mut Game, depth: u8, a: Value, b: Value) -> Value {
        if depth == 0 || game.over() {
            return self.static_val(game);
        }
        let (_, v) = self.search(game, depth, a, b);
        v
    }
}
