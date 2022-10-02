use std::collections::VecDeque;
use std::iter;

pub type Coord = (i16, i16);

pub struct Canvas {
    data: VecDeque<VecDeque<char>>,
    origin: Coord,
    width: usize,
}

fn blank_line(width: usize) -> VecDeque<char> {
    iter::repeat(' ').take(width).collect()
}

// fn blank_lines(height: usize, width: usize) -> VecDeque<VecDeque<char>> {
//     iter::repeat_with(|| blank_line(width))
//         .take(height)
//         .collect()
// }

impl Canvas {
    pub fn new() -> Canvas {
        Canvas {
            data: VecDeque::new(),
            origin: (0, 0),
            width: 0,
        }
    }

    pub fn put(&mut self, pos: Coord, v: char) {
        let idx = (self.origin.0 + pos.0, self.origin.1 + pos.1);
        for _ in idx.0..0 {
            for row in self.data.iter_mut() {
                row.push_front(' ');
            }
            self.width += 1;
            self.origin.0 += 1;
        }
        for _ in self.width as i16..=idx.0 {
            for row in self.data.iter_mut() {
                row.push_back(' ');
            }
            self.width += 1;
        }
        for _ in idx.1..0 {
            self.data.push_front(blank_line(self.width));
            self.origin.1 += 1;
        }
        for _ in self.data.len() as i16..=idx.1 {
            self.data.push_back(blank_line(self.width));
        }
        let idx = (self.origin.0 + pos.0, self.origin.1 + pos.1);
        self.data[idx.1 as usize][idx.0 as usize] = v;
    }

    pub fn render(self) -> String {
        self.data
            .into_iter()
            .flat_map(|l| l.into_iter().chain(iter::once('\n')))
            .collect()
    }

    pub fn put_tile(&mut self, (x, y): Coord, l1: [char; 3], l2: [char; 3]) {
        self.put((x + 1, y), '🮣');
        for i in 2..5 {
            self.put((x + i, y), '─');
        }
        self.put((x + 5, y), '🮢');

        self.put((x, y + 1), '🮣');
        self.put((x + 1, y + 1), '🮠');
        for i in 0..3 {
            self.put((x + 2 + (i as i16), y + 1), l1[i]);
        }
        self.put((x + 5, y + 1), '🮡');
        self.put((x + 6, y + 1), '🮢');

        self.put((x, y + 2), '🮡');
        self.put((x + 1, y + 2), '🮢');
        for i in 0..3 {
            self.put((x + 2 + (i as i16), y + 2), l2[i]);
        }
        self.put((x + 5, y + 2), '🮣');
        self.put((x + 6, y + 2), '🮠');

        self.put((x + 1, y + 3), '🮡');
        for i in 2..5 {
            self.put((x + i, y + 3), '─');
        }
        self.put((x + 5, y + 3), '🮠');
    }
}
