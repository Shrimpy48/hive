use std::sync::{mpsc, Arc, RwLock};
use std::thread;

use eframe::egui;
use egui_extras::RetainedImage;
use enum_map::{enum_map, EnumMap};

use hive::{Game, Move, PieceType, Pos};

mod engine;
mod widgets;

use engine::Engine;
use widgets::{game, toggle};

fn load_piece_icons() -> EnumMap<PieceType, RetainedImage> {
    enum_map! {
        PieceType::Ant => {
            RetainedImage::from_svg_str("ant.svg", include_str!("resources/ant.svg")).unwrap()
        }
        PieceType::Beetle => {
            RetainedImage::from_svg_str("beetle.svg", include_str!("resources/beetle.svg")).unwrap()
        }
        PieceType::Hopper => {
            RetainedImage::from_svg_str("hopper.svg", include_str!("resources/hopper.svg")).unwrap()
        }
        PieceType::Queen => {
            RetainedImage::from_svg_str("queen.svg", include_str!("resources/queen.svg")).unwrap()
        }
        PieceType::Spider => {
            RetainedImage::from_svg_str("spider.svg", include_str!("resources/spider.svg")).unwrap()
        }
    }
}

fn main() {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "My egui App",
        options,
        Box::new(|cc| Box::new(GuiState::new(cc.egui_ctx.clone()))),
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Selection {
    Nothing,
    InHand(PieceType),
    InHive(Pos),
}

struct GuiState {
    game: Game,
    selection: Selection,
    icons: EnumMap<PieceType, RetainedImage>,
    engine_send: mpsc::Sender<engine::Msg>,
    engine_recv: Arc<RwLock<Move>>,
    engine_active: bool,
}

impl GuiState {
    fn new(ctx: egui::Context) -> Self {
        let (tx, rx) = mpsc::channel();
        let engine_recv = Arc::new(RwLock::new(Move::Skip));
        let engine_send = engine_recv.clone();
        // The thread will exit when tx is dropped.
        thread::spawn(|| Engine::new(rx, engine_send, move || ctx.request_repaint()).run());
        Self {
            game: Game::new(),
            selection: Selection::Nothing,
            icons: load_piece_icons(),
            engine_send: tx,
            engine_recv,
            engine_active: false,
        }
    }
}

impl eframe::App for GuiState {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let engine_move = if self.engine_active {
            *self.engine_recv.read().unwrap()
        } else {
            hive::Move::Skip
        };
        egui::TopBottomPanel::bottom("engine controls").show(ctx, |ui| {
            ui.label("engine");
            if ui.add(toggle(&mut self.engine_active)).changed() {
                if self.engine_active {
                    self.engine_send.send(engine::Msg::Start).unwrap();
                } else {
                    self.engine_send.send(engine::Msg::Pause).unwrap();
                }
            }
        });
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(game(
                &self.icons,
                &mut self.game,
                &mut self.selection,
                &self.engine_send,
                engine_move,
            ));
        });
    }
}
