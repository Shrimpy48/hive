use std::sync::{mpsc, Arc, RwLock};
use std::thread;

use eframe::egui;
use egui_extras::RetainedImage;
use enum_map::{enum_map, EnumMap};
use rand::{prelude::*, thread_rng};

use hive::{Colour, Game, Move, PieceType, Pos};

mod engine;
mod widgets;

use engine::Engine;
use widgets::{hand, hive, toggle};

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
        Box::new(|_cc| Box::new(GuiState::default())),
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
    fn show_game(&mut self, ui: &mut egui::Ui) {
        egui::TopBottomPanel::top("black hand").show_inside(ui, |ui| {
            ui.centered_and_justified(|ui| {
                ui.add(hand(
                    &self.icons,
                    Colour::Black,
                    self.game.black_hand(),
                    self.game.turn(),
                    &mut self.selection,
                ));
            });
        });
        egui::TopBottomPanel::bottom("white hand").show_inside(ui, |ui| {
            ui.centered_and_justified(|ui| {
                ui.add(hand(
                    &self.icons,
                    Colour::White,
                    self.game.white_hand(),
                    self.game.turn(),
                    &mut self.selection,
                ));
            });
        });
        egui::CentralPanel::default().show_inside(ui, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.centered_and_justified(|ui| {
                    ui.add(hive(
                        &self.icons,
                        &mut self.game,
                        &mut self.selection,
                        &self.engine_send,
                    ));
                });
            });
        });
    }
}

impl Default for GuiState {
    fn default() -> Self {
        let (tx, rx) = mpsc::channel();
        let engine_recv = Arc::new(RwLock::new(Move::Skip));
        let engine_send = engine_recv.clone();
        // The thread will exit when tx is dropped.
        thread::spawn(|| Engine::new(rx, engine_send).run());
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
        egui::TopBottomPanel::top("engine controls").show(ctx, |ui| {
            if ui.add(toggle(&mut self.engine_active)).changed() {
                if self.engine_active {
                    self.engine_send.send(engine::Msg::Start).unwrap();
                } else {
                    self.engine_send.send(engine::Msg::Pause).unwrap();
                }
            }
            // TODO: properly notify egui of changes to this.
            ui.label(format!("{:?}", self.engine_recv.read().unwrap()));
        });
        egui::CentralPanel::default().show(ctx, |ui| self.show_game(ui));
    }
}
