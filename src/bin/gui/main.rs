use eframe::egui;
use egui_extras::RetainedImage;

use enum_map::{enum_map, EnumMap};
use hive::{Colour, Game, PieceType, Pos};

mod widgets;

use rand::{prelude::*, thread_rng};
use widgets::{hand, hive};

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

#[derive(Debug, Clone, Copy)]
enum Selection {
    Nothing,
    InHand(PieceType),
    InHive(Pos),
}

struct GuiState {
    game: Game,
    selection: Selection,
    icons: EnumMap<PieceType, RetainedImage>,
}

impl Default for GuiState {
    fn default() -> Self {
        Self {
            game: Game::new(),
            selection: Selection::Nothing,
            icons: load_piece_icons(),
        }
    }
}

impl eframe::App for GuiState {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
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
        egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
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
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.centered_and_justified(|ui| {
                    ui.add(hive(&self.icons, &mut self.game, &mut self.selection));
                });
            });
        });
    }
}
