use crate::game::*;
use crate::hive::{Colour, Piece, PieceType, Pos};
use std::collections::hash_map::Entry;
use ahash::AHashMap;
use rand::{thread_rng, Rng};
use rand::rngs::ThreadRng;
use std::cmp::{Ord, Ordering};
use std::fmt;

#[derive(Debug, Eq, Clone, Copy)]
enum Value {
    Win(u32),
    Loss(u32),
    Value(i32)
}

impl Value {
    fn fwd(&self) -> Self {
        match self {
            Value::Win(i) => Value::Loss(i-1),
            Value::Loss(i) => Value::Win(i-1),
            Value::Value(i) => Value::Value(-i)
        }
    }

    fn back(&self) -> Self {
        match self {
            Value::Win(i) => Value::Loss(i+1),
            Value::Loss(i) => Value::Win(i+1),
            Value::Value(i) => Value::Value(-i)
        }
    }

    fn flip(&self) -> Self {
        match self {
            Value::Win(i) => Value::Loss(*i),
            Value::Loss(i) => Value::Win(*i),
            Value::Value(i) => Value::Value(-i)
        }
    }

    fn pred(&self) -> Self {
        match self {
            Value::Win(i) => Value::Win(i+1),
            Value::Loss(i) => Value::Loss(i-1),
            Value::Value(i) => Value::Value(i-1)
        }
    }

    fn succ(&self) -> Self {
        match self {
            Value::Win(i) => Value::Win(i-1),
            Value::Loss(i) => Value::Loss(i+1),
            Value::Value(i) => Value::Value(i+1)
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Win(i), Value::Win(j)) => i == j,
            (Value::Loss(i), Value::Loss(j)) => i == j,
            (Value::Value(i), Value::Value(j)) => i == j,
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Win(i), Value::Win(j)) => j.cmp(i),
            (Value::Win(_), _) => Ordering::Greater,
            (_, Value::Win(_)) => Ordering::Less,
            (Value::Loss(i), Value::Loss(j)) => i.cmp(j),
            (Value::Loss(_), _) => Ordering::Less,
            (_, Value::Loss(_)) => Ordering::Greater,
            (Value::Value(i), Value::Value(j)) => i.cmp(j),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Win(i) => write!(f, "+M{}", (i+1)/2),
            Value::Loss(i) => write!(f, "-M{}", (i+1)/2),
            Value::Value(0) => write!(f, "0"),
            Value::Value(i) => write!(f, "{:+}", i)
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct MaybeValue(Option<Value>);

impl MaybeValue {
    const EMPTY: MaybeValue = MaybeValue(None);

    fn greater(&self, v: Value) -> Value {
        match self {
            MaybeValue(None) => v,
            MaybeValue(Some(x)) => *x.max(&v)
        }
    }

    fn lesser(&self, v: Value) -> Value {
        match self {
            MaybeValue(None) => v,
            MaybeValue(Some(x)) => *x.min(&v)
        }
    }

    fn above(&self, b: Value) -> bool {
        match self {
            MaybeValue(None) => Value::Loss(1) >= b,
            MaybeValue(Some(x)) => x >= &b
        }
    }

    fn below(&self, a: Value) -> bool {
        match self {
            MaybeValue(None) => Value::Win(1) <= a,
            MaybeValue(Some(x)) => x <= &a
        }
    }

    fn fwd(&self) -> Self {
        match self {
            MaybeValue(Some(Value::Loss(0))) => MaybeValue(None),
            MaybeValue(Some(Value::Win(0))) => MaybeValue(None),
            MaybeValue(x) => MaybeValue(x.map(|v| v.fwd()))
        }
    }

    fn from(v: Value) -> MaybeValue {
        MaybeValue(Some(v))
    }
}

#[derive(Debug, Clone, Copy)]
enum TValue {
    Exact(Value),
    UpperBound(Value),
    LowerBound(Value),
}

impl TValue {
    fn is_exact(&self) -> bool {
        match self {
            TValue::Exact(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct TEntry {
    d: u8,
    val: TValue,
    ms: Vec<Move>,
}

struct TTable {
    data: AHashMap<u64, TEntry>,
    max_size: usize,
    rng: ThreadRng
}

impl TTable {
    fn new(max_size: usize) -> TTable {
        TTable {
            data: AHashMap::with_capacity(max_size),
            max_size,
            rng: thread_rng()
        }
    }

    fn get(&self, game: &mut Game) -> Option<&TEntry> {
        let key = game.table_key();
        self.data.get(&key)
    }

    fn insert(&mut self, game: &mut Game, entry: TEntry) {
        let key = game.table_key();
        match self.data.entry(key) {
            Entry::Occupied(mut oe) => {
                let &TEntry { d, val: _, ms: _ } = oe.get();
                if entry.d > d {
                    oe.insert(entry);
                } else if entry.d == d && entry.val.is_exact() {
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

pub struct Bot {
    table: TTable,
}

impl Bot {
    pub fn new(table_size: usize) -> Bot {
        Bot {
            table: TTable::new(table_size),
        }
    }

    pub fn make_best_move(&mut self, game: &mut Game, depth: u8) {
        let ms = game.moves();
        if ms.len() == 1 {
            game.make_move(ms[0]);
            return;
        }
        let mut best = Move::Skip;
        for d in 1..=depth {
            let (m, v) = self.search(game, d, MaybeValue::EMPTY, MaybeValue::EMPTY);
            let v_ = match game.turn {
                Colour::White => v,
                Colour::Black => v.flip()
            };
            println!("{}: {}", v_, m);
            best = m;
        }
        game.make_move(best);
    }

    fn act_val(piece: PieceType) -> i32 {
        match piece {
            PieceType::Queen => 16,
            PieceType::Beetle => 2,
            PieceType::Hopper => 2,
            PieceType::Spider => 2,
            PieceType::Ant => 4,
            // PieceType::Ladybug => 1,
            // PieceType::Mosquito => 1,
            // PieceType::Pillbug => 1,
        }
    }

    fn static_val(&self, game: &Game) -> Value {
        match game.result() {
            Result::Win(Colour::White) => match game.turn {
                Colour::White => Value::Win(0),
                Colour::Black => Value::Loss(0),
            }
            Result::Win(Colour::Black) => match game.turn {
                Colour::White => Value::Loss(0),
                Colour::Black => Value::Win(0),
            }
            Result::Draw => Value::Value(0),
            Result::Ongoing => {
                let (white_wnc, white_bnc) = match game.white_queen {
                    None => (0, 0),
                    Some(pos) => {
                        let (wn, bn): (Vec<&Piece>, Vec<&Piece>) = game
                            .hive
                            .neighbours(pos, None)
                            .partition(|n| n.col == Colour::White);
                        (wn.len() as i32, bn.len() as i32)
                    }
                };
                let (black_wnc, black_bnc) = match game.black_queen {
                    None => (0, 0),
                    Some(pos) => {
                        let (wn, bn): (Vec<&Piece>, Vec<&Piece>) = game
                            .hive
                            .neighbours(pos, None)
                            .partition(|n| n.col == Colour::White);
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
                let (white_pos_vec, black_pos_vec): (Vec<(&Pos, &Piece)>, Vec<(&Pos, &Piece)>) =
                    game.hive.tiles().partition(|(_, p)| p.col == Colour::White);
                let white_act: i32 = white_pos_vec
                    .into_iter()
                    .map(|(&pos, piece)| {
                        Self::act_val(piece.typ)
                            * game.hive.destinations(pos, piece.typ).len() as i32
                    })
                    .sum();
                let black_act: i32 = black_pos_vec
                    .into_iter()
                    .map(|(&pos, piece)| {
                        Self::act_val(piece.typ)
                            * game.hive.destinations(pos, piece.typ).len() as i32
                    })
                    .sum();
                // pieces on board
                // pieces that can/cannot be moved
                // 	where/how many places
                // pieces around queen
                // whether queen can move
                // how enclosed open spaces around the queen are
                let white_val = 64 * (black_bnc - 1 + 3 * black_wnc) + white_act; // + white_mat;
                let black_val = 64 * (white_wnc - 1 + 3 * white_bnc) + black_act; // + black_mat;
                match game.turn {
                    Colour::White => Value::Value(white_val - black_val),
                    Colour::Black => Value::Value(black_val - white_val),
                }
            }
        }
    }

    fn search(&mut self, game: &mut Game, depth: u8, a: MaybeValue, b: MaybeValue) -> (Move, Value) {
        let mut moves;
        let start_a = a;
        if let Some(TEntry { d, val, ms }) = self.table.get(game) {
            if *d >= depth {
                match val {
                    TValue::Exact(v) => return (ms[0], *v),
                    TValue::LowerBound(v) => {
                        let a_ = a.greater(*v);
                        if b.below(a_) {
                            return (ms[0], *v);
                        }
                    }
                    TValue::UpperBound(v) => {
                        let b_ = b.lesser(*v);
                        if a.above(b_) {
                            return (ms[0], *v);
                        }
                    }
                }
            }
            moves = ms.clone();
            let pv = moves[0];
            let mut best = 0;
            let m_ = game.make_move(pv);
            let mut value = self.val(game, depth - 1, b.fwd(), a.fwd()).back();
            game.unmake_move(m_);
            let mut a_ = a.greater(value);
            if !b.below(a_) {
                for (i, m) in moves[1..].iter().enumerate() {
                    let m_ = game.make_move(*m);
                    let mut val = self.val(game, depth - 1, MaybeValue::from(a_.succ()).fwd(), MaybeValue::from(a_.fwd())).back();
                    if a_ < val && !b.below(val) {
                        val = self.val(game, depth - 1, b.fwd(), MaybeValue::from(val.fwd())).back();
                    }
                    game.unmake_move(m_);
                    if value < val {
                        value = val;
                        best = i+1;
                    }
                    a_ = a_.max(value);
                    if b.below(a_) {
                        break;
                    }
                }
            }
            let tval = if start_a.above(value) {
                TValue::UpperBound(value)
            } else if b.below(value) {
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

        moves = game.moves();
        let fst = moves[0];
        let mut best = 0;
        let m_ = game.make_move(fst);
        let mut value = self.val(game, depth - 1, b.fwd(), a.fwd()).back();
        game.unmake_move(m_);
        let mut a_ = a.greater(value);
        if !b.below(a_) {
            for (i, m) in moves[1..].iter().enumerate() {
                let m_ = game.make_move(*m);
                let val = self.val(game, depth - 1, b.fwd(), MaybeValue::from(a_.fwd())).back();
                game.unmake_move(m_);
                if value < val {
                    value = val;
                    best = i+1;
                }
                a_ = a_.max(value);
                if b.below(a_) {
                    break;
                }
            }
        }

        let tval = if start_a.above(value) {
            TValue::UpperBound(value)
        } else if b.below(value) {
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

    fn val(&mut self, game: &mut Game, depth: u8, a: MaybeValue, b: MaybeValue) -> Value {
        if depth == 0 || game.over() {
            return self.static_val(game);
        }
        let (_, v) = self.search(game, depth, a, b);
        return v;
    }
}
