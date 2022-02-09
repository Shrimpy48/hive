use std::cmp::max;
use crate::model::*;
use std::collections::HashMap;
// use std::collections::BTreeSet;
use std::thread;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::channel;
use std::time::Duration;
// use std::time::Instant;

mod model;

enum TableScore {
    Exact(i32),
    UpperBound(i32),
    LowerBound(i32),
}

fn main() {
    let mut game = Game::new();
    let a = i32::MIN + 1;  // So that it can be negated
    let b = i32::MAX;
    let cache_ref = Arc::new(Mutex::new(HashMap::new()));
    while !game.game_over() {
        let term_ref = Arc::new(Mutex::new(false));
        let cache_ref2 = Arc::clone(&cache_ref);
        let term_ref2 = Arc::clone(&term_ref);
        let game_r = game.clone();
        let (move_tx, move_rx) = channel();
        let work_h = thread::spawn(move || {
            let cache_mtx = &*cache_ref2;
            let term = &*term_ref2;
            let mut cache = cache_mtx.lock().unwrap();
            for depth in 1.. {
                let (i, _) = pvs(&mut cache, term, &game_r, depth, a, b);
                let done = term.lock().unwrap();
                if !*done {
                    move_tx.send(i).unwrap();
                } else {
                    break;
                }
            }
        });
        let term_ref3 = Arc::clone(&term_ref);
        let timeout_h = thread::spawn(move || {
            let term = &*term_ref3;
            thread::sleep(Duration::from_secs(5));
            let mut done = term.lock().unwrap();
            *done = true;
        });

        println!("{}", game);
        let mut best = 0;
        for i in move_rx {
            best = i;
        }
        game.make_move(game.moves()[best]);

        timeout_h.join().unwrap();
        work_h.join().unwrap();
    }
}

// fn main() {
//     let game = Game::new();
//     let a = i32::MIN + 1;  // So that it can be negated
//     let b = i32::MAX;
//     let mut cache = HashMap::new();
//     let mut cache2 = HashMap::new();
//     let term = Mutex::new(false);
//     for depth in 1.. {
//         println!("{}", depth);
//         let start = Instant::now();
//         let (i, _) = pvs(&mut cache, &term, &game, depth, a, b);
//         let end = Instant::now();
//         println!("{:?}", i);
//         println!("{:?}", end - start);
//         let start = Instant::now();
//         let (mvs, _) = pvs_list(&mut cache2, &term, &game, depth, a, b);
//         let end = Instant::now();
//         println!("{:?}", mvs);
//         println!("{:?}", end - start);
//     }
// }

const SQUAREWEIGHTS: [[i32; 8]; 8] = [
    [120, -20, 20,  5,  5, 20, -20, 120],
    [-20, -40, -5, -5, -5, -5, -40, -20],
    [ 20,  -5, 15,  3,  3, 15,  -5,  20],
    [  5,  -5,  3,  3,  3,  3,  -5,   5],
    [  5,  -5,  3,  3,  3,  3,  -5,   5],
    [ 20,  -5, 15,  3,  3, 15,  -5,  20],
    [-20, -40, -5, -5, -5, -5, -40, -20],
    [120, -20, 20,  5,  5, 20, -20, 120]
];

fn static_val(node: &Game) -> i32 {
    if node.game_over() {
        return i32::MAX * node.result();
    }
    let mut val = 0;
    for r in 0..8 {
        for c in 0..8 {
            val += node.board[r][c] * SQUAREWEIGHTS[r][c];
        }
    }
    return val;
}

fn pvs(cache: &mut HashMap<Game, (u32, usize, TableScore)>, term: &Mutex<bool>, node: &Game, depth: u32, mut a: i32, b: i32) -> (usize, i32) {
    {
        let done = term.lock().unwrap();
        if *done {
            return (5000, 0);
        }
    }
    let mut is_upper = true;
    let mut moves;
    let mut swapped_i = 0;
    let mut should_write = true;
    if let Some((d, i, v)) = cache.get(node) {
        if *d >= depth {
            match *v {
                TableScore::Exact(val) => {
                    return (*i, val);
                }
                TableScore::UpperBound(val) => {
                    if val <= a {
                        return (*i, a);
                    }
                    // Could also avoid comparing a and b if val < b
                }
                TableScore::LowerBound(val) => {
                    a = max(a, val);
                    if a >= b {
                        return (*i, a);
                    }
                }
            }
        }
        if *d > depth {
            should_write = false;
        }
        moves = node.moves();
        moves.swap(0, *i);
        swapped_i = *i;
    } else {
        moves = node.moves();
    }
    let mut best = 0;
    let mut i = 0;
    let mut it = moves.iter();
    if let Some(m) = it.next() {
        let mut child = node.clone();
        child.make_move(*m);
        let score = -val(cache, term, &child, depth - 1, -b, -a);
        if score > a {
            is_upper = false;
            a = score;
            best = swapped_i;
        }
        if a >= b {
            if should_write {
                cache.insert(node.clone(), (depth, best, TableScore::LowerBound(a)));
            }
            return (best, a);
        }
        i += 1;
    }
    for m in it {
        let mut child = node.clone();
        child.make_move(*m);
        let mut score = -val(cache, term, &child, depth - 1, -a - 1, -a);
        if a < score && score < b {
            score = -val(cache, term, &child, depth - 1, -b, -score)
        }
        if score > a {
            is_upper = false;
            a = score;
            if i == swapped_i {
                best = 0;
            } else {
                best = i;
            }
        }
        if a >= b {
            if should_write {
                cache.insert(node.clone(), (depth, best, TableScore::LowerBound(a)));
            }
            return (best, a);
        }
        i += 1;
    }
    if should_write {
        if is_upper {
            cache.insert(node.clone(), (depth, best, TableScore::UpperBound(a)));
        } else {
            cache.insert(node.clone(), (depth, best, TableScore::Exact(a)));
        }
    }
    return (best, a);
}

fn val(cache: &mut HashMap<Game, (u32, usize, TableScore)>, term: &Mutex<bool>, node: &Game, depth: u32, a: i32, b: i32) -> i32 {
    if depth == 0 || node.game_over() {
        let val = static_val(node) * node.turn;
        return val;
    }
    let (_, val) = pvs(cache, term, node, depth, a, b);
    return val;
}

// fn pvs_list(cache: &mut HashMap<Game, (u32, Vec<Move>, TableScore)>, term: &Mutex<bool>, node: &Game, depth: u32, mut a: i32, b: i32) -> (Vec<Move>, i32) {
//     {
//         let done = term.lock().unwrap();
//         if *done {
//             return (Vec::new(), 0);
//         }
//     }
//     let mut is_upper = true;
//     let moves;
//     let mut should_write = true;
//     if let Some((d, mvs, v)) = cache.get(node) {
//         if *d >= depth {
//             match *v {
//                 TableScore::Exact(val) => {
//                     return (mvs.clone(), val);
//                 }
//                 TableScore::UpperBound(val) => {
//                     if val <= a {
//                         return (mvs.clone(), a);
//                     }
//                     // Could also avoid comparing a and b if val < b
//                 }
//                 TableScore::LowerBound(val) => {
//                     a = max(a, val);
//                     if a >= b {
//                         return (mvs.clone(), a);
//                     }
//                 }
//             }
//         }
//         if *d > depth {
//             should_write = false;
//         }
//         moves = mvs.clone();
//     } else {
//         moves = node.moves();
//     }
//     let mut bests = BTreeSet::new();
//     let mut it = moves.iter();
//     if let Some(m) = it.next() {
//         let mut child = node.clone();
//         child.make_move(*m);
//         let score = -val_list(cache, term, &child, depth - 1, -b, -a);
//         if score > a {
//             is_upper = false;
//             a = score;
//         }
//         bests.insert((score, *m));
//         if a >= b {
//             let mut bests_vec: Vec<Move> = bests.iter().map(|(_, m)| *m).rev().collect();
//             bests_vec.extend(it);
//             if should_write {
//                 cache.insert(node.clone(), (depth, bests_vec.clone(), TableScore::LowerBound(a)));
//             }
//             return (bests_vec, a);
//         }
//     }
//     loop {
//         if let Some(m) = it.next() {
//             let mut child = node.clone();
//             child.make_move(*m);
//             let mut score = -val_list(cache, term, &child, depth - 1, -a - 1, -a);
//             if a < score && score < b {
//                 score = -val_list(cache, term, &child, depth - 1, -b, -score)
//             }
//             if score > a {
//                 is_upper = false;
//                 a = score;
//             }
//             bests.insert((score, *m));
//             if a >= b {
//                 let mut bests_vec: Vec<Move> = bests.iter().map(|(_, m)| *m).rev().collect();
//                 bests_vec.extend(it);
//                 if should_write {
//                     cache.insert(node.clone(), (depth, bests_vec.clone(), TableScore::LowerBound(a)));
//                 }
//                 return (bests_vec, a);
//             }
//         } else {
//             break;
//         }
//     }
//     let bests_vec: Vec<Move> = bests.iter().map(|(_, m)| *m).rev().collect();
//     if should_write {
//         if is_upper {
//             cache.insert(node.clone(), (depth, bests_vec.clone(), TableScore::UpperBound(a)));
//         } else {
//             cache.insert(node.clone(), (depth, bests_vec.clone(), TableScore::Exact(a)));
//         }
//     }
//     return (bests_vec, a);
// }

// fn val_list(cache: &mut HashMap<Game, (u32, Vec<Move>, TableScore)>, term: &Mutex<bool>, node: &Game, depth: u32, a: i32, b: i32) -> i32 {
//     if depth == 0 || node.game_over() {
//         let val = static_val(node) * node.turn;
//         return val;
//     }
//     let (_, val) = pvs_list(cache, term, node, depth, a, b);
//     return val;
// }
