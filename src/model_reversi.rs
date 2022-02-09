use std::fmt;
use std::cmp::Ordering;

type Square = i32;

type Board = [[Square; 8]; 8];

type Pos = (usize, usize);

pub type Move = Option<Pos>;

type Colour = i32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Game {
    pub board: Board,
    pub turn: Colour
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(str_of(self.board[0][0]))?;
        for c in 1..8 {
            write!(f, " {}", str_of(self.board[0][c]))?;
        }
        for r in 1..8 {
            write!(f, "\n{}", str_of(self.board[r][0]))?;
            for c in 1..8 {
                write!(f, " {}", str_of(self.board[r][c]))?;
            }
        }
        return Ok(());
    }
}

fn str_of(s: Square) -> &'static str {
    match s {
        0 => " ",
        1 => "W",
        -1 => "B",
        _ => "X"
    }
}

impl Game {
    pub fn moves(&self) -> Vec<Move> {
        let mvs: Vec<Move> = (0..8).flat_map(|r| (0..8).map(move |c| (r, c)))
            .filter(|m| self.is_valid(*m))
            .map(|m| Some(m))
            .collect();
        if mvs.is_empty() {
            vec![None]
        } else {
            mvs
        }
    }

    pub fn make_move(&mut self, m: Move) {
        if let Some((r,c)) = m {
            self.board[r][c] = self.turn;
            for d in &[(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)] {
                if self.makes_line(*d, (r,c)) {
                    self.flip_line(*d, (r,c));
                }
            }
        }
        self.turn *= -1;
    }

    pub fn game_over(&self) -> bool {
        if !self.moves().is_empty() {
            return false;
        }
        let mut next = self.clone();
        next.make_move(None);
        return next.moves().is_empty();
    }

    pub fn result(&self) -> i32 {
        let white: usize = self.board.iter().map(|r| r.iter().filter(|&&s| s==1).count()).sum();
        let black: usize = self.board.iter().map(|r| r.iter().filter(|&&s| s==-1).count()).sum();
        match white.cmp(&black) {
            Ordering::Greater => 1,
            Ordering::Equal => 0,
            Ordering::Less => -1
        }
    }

    pub fn new() -> Game {
        Game {
            board: [[0, 0, 0,  0,  0, 0, 0, 0] ,
                    [0, 0, 0,  0,  0, 0, 0, 0] ,
                    [0, 0, 0,  0,  0, 0, 0, 0] ,
                    [0, 0, 0,  1, -1, 0, 0, 0] ,
                    [0, 0, 0, -1,  1, 0, 0, 0] ,
                    [0, 0, 0,  0,  0, 0, 0, 0] ,
                    [0, 0, 0,  0,  0, 0, 0, 0] ,
                    [0, 0, 0,  0,  0, 0, 0, 0]],
            turn: -1,
        }
    }

    // pub fn is_legal_move(&self, m: Pos) -> bool {
    //     self.is_in_bounds(m) && self.is_valid(m)
    // }

    // fn is_in_bounds(&self, p: Pos) -> bool {
    //     let (r, c) = p;
    //     r < 8 && c < 8
    // }

    fn is_in_bounds_signed(&self, p: (i32, i32)) -> bool {
        let (r, c) = p;
        0 <= r && r < 8 && 0 <= c && c < 8
    }

    fn is_valid(&self, m: Pos) -> bool {
        let (r, c) = m;
        if self.board[r][c] != 0 {
            return false;
        }
        for d in &[(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)] {
            if self.makes_line(*d, m) {
                return true;
            }
        }
        return false;
    }

    fn makes_line(&self, d: (i32, i32), m: Pos) -> bool {
        let mut has_opp = false;
        let mut pos = (m.0 as i32 + d.0, m.1 as i32 + d.1);
        while self.is_in_bounds_signed(pos) {
            let (r, c) = (pos.0 as usize, pos.1 as usize);
            match self.board[r][c] {
                0 => { return false; }
                col => {
                    if col == self.turn {
                        return has_opp;
                    } else {
                        has_opp = true;
                    }
                }
            }
            pos = (pos.0 + d.0, pos.1 + d.1);
        }
        return false;
    }

    fn flip_line(&mut self, d: (i32, i32), m: Pos) {
        let mut pos = (m.0 as i32 + d.0, m.1 as i32 + d.1);
        while self.is_in_bounds_signed(pos) {
            let (r, c) = (pos.0 as usize, pos.1 as usize);
            match self.board[r][c] {
                0 => { return; }
                col => {
                    if col == self.turn {
                        return;
                    } else {
                        self.board[r][c] = self.turn;
                    }
                }
            }
            pos = (pos.0 + d.0, pos.1 + d.1);
        }
    }
}
