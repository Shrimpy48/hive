//! A Monte Carlo Tree Search agent for Hive, based on the one we developed for Hex.

use rand::prelude::*;
use rand::rngs::SmallRng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::f64::consts::LN_2;
use std::rc::{Rc, Weak};
use std::time::Instant;

use crate::game::{Game, Move, Outcome};

const EXPLORE_C: f64 = 1.0;
const BATCH_SIZE: u32 = 1;

/// A node in the search tree.
/// Uses reference counting to easily ensure memory safety.
#[derive(Debug)]
struct SearchNode {
    simulations: u64,
    wins: u64,
    parent: Option<(Move, Weak<RefCell<SearchNode>>)>,
    children: HashMap<Move, Rc<RefCell<SearchNode>>>,
    unexpanded: Vec<Move>,
}

impl SearchNode {
    fn new() -> Self {
        let game = Game::new();
        let unexpanded = game.moves();
        Self {
            children: Default::default(),
            parent: None,
            wins: 0,
            simulations: 0,
            unexpanded,
        }
    }

    /// Panics if simulations == 0.
    fn exploit(&self) -> f64 {
        self.wins as f64 / self.simulations as f64
    }

    /// Panics if called on the root node or if simulations == 0.
    fn explore(&self) -> f64 {
        let parent_sims = self
            .parent
            .as_ref()
            .expect("cannot explore root node")
            .1
            .upgrade()
            .expect("parent freed before child")
            .borrow()
            .simulations as f64;
        (2.0 * parent_sims.log2() * LN_2 / self.simulations as f64).sqrt()
    }

    fn ucb(&self) -> f64 {
        self.exploit() + EXPLORE_C * self.explore()
    }

    fn lcb(&self) -> f64 {
        self.exploit() - EXPLORE_C * self.explore()
    }

    /// Create a new child of this SearchNode.
    fn after_move(this: Rc<RefCell<Self>>, game: &mut Game, made: Move) -> Rc<RefCell<Self>> {
        let out;
        {
            game.make_move_unchecked(made);
            let unexpanded = game.moves();
            out = Rc::new(RefCell::new(Self {
                children: Default::default(),
                parent: Some((made, Rc::downgrade(&this))),
                unexpanded,
                simulations: 0,
                wins: 0,
            }));
        }
        let mut borrowed = this.borrow_mut();
        borrowed.children.insert(made, Rc::clone(&out));
        // let idx = borrowed.unexpanded.binary_search(&made).expect("node already expanded");
        // borrowed.unexpanded.remove(idx);
        let idx = borrowed
            .unexpanded
            .iter()
            .enumerate()
            .find(|&(_, &x)| x == made)
            .map(|(i, _)| i)
            .expect("position not empty");
        borrowed.unexpanded.swap_remove(idx);
        out
    }

    fn expandable(&self) -> bool {
        !self.unexpanded.is_empty()
    }
}

enum SelectRes {
    Terminal(Rc<RefCell<SearchNode>>),
    NonTerminal(Rc<RefCell<SearchNode>>),
}

#[derive(Debug)]
pub struct Bot {
    game: Game,
    subtree: Rc<RefCell<SearchNode>>,
    rng: SmallRng,
}

impl Bot {
    pub fn new() -> Self {
        Self {
            game: Game::new(),
            subtree: Rc::new(RefCell::new(SearchNode::new())),
            rng: SmallRng::from_entropy(),
        }
    }

    pub fn discard_others(&mut self, made: Move) {
        // Making sure that the borrow is dropped before after_move is called
        let new_subtree = self.subtree.borrow().children.get(&made).map(Rc::clone);
        if let Some(node) = new_subtree {
            self.game.make_move_unchecked(made);
            node.borrow_mut().parent = None;
            self.subtree = node;
            return;
        }

        let new_subtree = SearchNode::after_move(Rc::clone(&self.subtree), &mut self.game, made);
        new_subtree.borrow_mut().parent = None;
        self.subtree = new_subtree;
    }

    pub fn mcts(&mut self, deadline: Instant) -> Move {
        loop {
            for _ in 0..BATCH_SIZE {
                self.search_step();
            }
            if Instant::now() >= deadline {
                eprintln!("Performed {} iterations", self.subtree.borrow().simulations);
                return self.best_move();
            }
        }
    }

    fn search_step(&mut self) {
        let node = self.to_simulate_from();
        let result = self.simulate();
        self.backpropagate(node, result);
    }

    /// Get a node to simulate from, possibly by expanding the search tree.
    fn to_simulate_from(&mut self) -> Rc<RefCell<SearchNode>> {
        if self.subtree.borrow().simulations == 0 {
            return Rc::clone(&self.subtree);
        }
        match self.select(Rc::clone(&self.subtree)) {
            SelectRes::Terminal(node) => node,
            SelectRes::NonTerminal(node) => self.expand(node),
        }
    }

    fn select(&mut self, node: Rc<RefCell<SearchNode>>) -> SelectRes {
        if node.borrow().expandable() {
            return SelectRes::NonTerminal(node);
        }

        let best = node
            .borrow()
            .children
            .iter()
            .map(|(m, x)| {
                let ucb = x.borrow().ucb();
                (m, x, ucb)
            })
            .min_by(|(_, _, a), (_, _, b)| a.total_cmp(b))
            .map(|(m, n, _)| (*m, Rc::clone(n)));
        match best {
            None => SelectRes::Terminal(node),
            Some((m, best_child)) => {
                self.game.make_move_unchecked(m);
                self.select(Rc::clone(&best_child))
            }
        }
    }

    fn expand(&mut self, node: Rc<RefCell<SearchNode>>) -> Rc<RefCell<SearchNode>> {
        let m = *node
            .borrow()
            .unexpanded
            .choose(&mut self.rng)
            .expect("no children to expand");
        SearchNode::after_move(node, &mut self.game, m)
    }

    fn simulate(&mut self) -> Outcome {
        let mut moves = Vec::new();
        while !self.game.over() {
            let m = *self
                .game
                .moves()
                .choose(&mut self.rng)
                .expect("no moves to make");
            self.game.make_move_unchecked(m);
            moves.push(m);
        }
        let out = self.game.result();
        for m in moves.into_iter().rev() {
            self.game.unmake_move_unchecked(m);
        }
        out
    }

    fn backpropagate(&mut self, node: Rc<RefCell<SearchNode>>, result: Outcome) {
        let mut current = Some(node);
        while let Some(n) = current {
            let mut n_mut = n.borrow_mut();
            n_mut.simulations += 1;
            if let Outcome::Win(player) = result {
                if self.game.turn() != player {
                    n_mut.wins += 1;
                }
            }
            current = n_mut.parent.as_ref().map(|(m, w)| {
                self.game.unmake_move_unchecked(*m);
                w.upgrade().expect("parent freed before child")
            });
        }
    }

    /// Panics if no search has been performed
    fn best_move(&self) -> Move {
        *self
            .subtree
            .borrow()
            .children
            .iter()
            // Manual max implementation since f64 is not totally ordered
            .map(|(c, x)| {
                let lcb = x.borrow().lcb();
                (c, lcb)
            })
            .min_by(|(_, a), (_, b)| a.total_cmp(b))
            .map(|(c, v)| {
                eprintln!("LCB: {:.1}%", v * 100.0);
                eprintln!("best move: {:?}", c);
                c
            })
            .unwrap_or_else(|| panic!("no search tree"))
    }
}

impl Default for Bot {
    fn default() -> Self {
        Self::new()
    }
}
