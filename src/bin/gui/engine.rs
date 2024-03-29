use std::sync::{mpsc, Arc, RwLock};
use std::time::{Duration, Instant};

use hive::bot::mcts::Bot;
use hive::Move;

#[derive(Debug, Clone, Copy)]
pub enum Msg {
    Pause,
    Start,
    Reset,
    MakeMove(Move),
}

pub struct Engine<F> {
    running: bool,
    recv: mpsc::Receiver<Msg>,
    send: Arc<RwLock<Move>>,
    notify: F,
    bot: Bot,
}

impl<F: FnMut()> Engine<F> {
    pub fn new(recv: mpsc::Receiver<Msg>, send: Arc<RwLock<Move>>, notify: F) -> Self {
        Self {
            running: false,
            recv,
            send,
            notify,
            bot: Bot::default(),
        }
    }

    pub fn run(mut self) {
        loop {
            if self.running {
                match self.recv.try_recv() {
                    Ok(Msg::Pause) => {
                        self.running = false;
                        continue;
                    }
                    Ok(Msg::Start) => {}
                    Ok(Msg::MakeMove(m)) => self.bot.discard_others(m),
                    Ok(Msg::Reset) => self.bot = Bot::new(),
                    Err(mpsc::TryRecvError::Disconnected) => return,
                    Err(mpsc::TryRecvError::Empty) => {}
                }

                let best_move = if self.bot.game().over() {
                    Move::Skip
                } else {
                    self.bot.mcts(Instant::now() + Duration::from_millis(250))
                };
                // Only block the UI thread if something has actually changed.
                if *self.send.read().unwrap() != best_move {
                    *self.send.write().unwrap() = best_move;
                    (self.notify)();
                }
            } else {
                // Just listen for messages
                match self.recv.recv() {
                    Ok(Msg::Pause) => {}
                    Ok(Msg::Start) => self.running = true,
                    Ok(Msg::MakeMove(m)) => self.bot.discard_others(m),
                    Ok(Msg::Reset) => self.bot = Bot::new(),
                    Err(mpsc::RecvError) => return,
                }
            }
        }
    }
}
