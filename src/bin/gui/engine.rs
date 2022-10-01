use std::sync::{mpsc, Arc, RwLock};
use std::time::{Duration, Instant};

use hive::bot::mcts::Bot;
use hive::Move;

#[derive(Debug, Clone, Copy)]
pub enum Msg {
    Pause,
    Start,
    MakeMove(Move),
}

pub struct Engine {
    running: bool,
    recv: mpsc::Receiver<Msg>,
    send: Arc<RwLock<Move>>,
    bot: Bot,
}

impl Engine {
    pub fn new(recv: mpsc::Receiver<Msg>, send: Arc<RwLock<Move>>) -> Self {
        Self {
            running: false,
            recv,
            send,
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
                    Err(mpsc::TryRecvError::Disconnected) => return,
                    Err(mpsc::TryRecvError::Empty) => {}
                }

                let best_move = self.bot.mcts(Instant::now() + Duration::from_secs(1));
                // Only block the UI thread if something has actually changed.
                if *self.send.read().unwrap() != best_move {
                    *self.send.write().unwrap() = best_move;
                }
            } else {
                // Just listen for messages
                match self.recv.recv() {
                    Ok(Msg::Pause) => {}
                    Ok(Msg::Start) => self.running = true,
                    Ok(Msg::MakeMove(m)) => self.bot.discard_others(m),
                    Err(mpsc::RecvError) => return,
                }
            }
        }
    }
}
