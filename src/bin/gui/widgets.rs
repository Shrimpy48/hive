//! Toggle switch code is from the egui documentation.
use std::sync::mpsc;

use eframe::egui;
use egui::epaint;
use enum_map::EnumMap;

use crate::engine;
use crate::Selection;

/// iOS-style toggle switch:
///
/// ``` text
///      _____________
///     /       /.....\
///    |       |.......|
///     \_______\_____/
/// ```
///
/// ## Example:
/// ``` ignore
/// toggle_ui(ui, &mut my_bool);
/// ```
pub(crate) fn toggle_ui(ui: &mut egui::Ui, on: &mut bool) -> egui::Response {
    // Widget code can be broken up in four steps:
    //  1. Decide a size for the widget
    //  2. Allocate space for it
    //  3. Handle interactions with the widget (if any)
    //  4. Paint the widget

    // 1. Deciding widget size:
    // You can query the `ui` how much space is available,
    // but in this example we have a fixed size widget based on the height of a standard button:
    let desired_size = ui.spacing().interact_size.y * egui::vec2(2.0, 1.0);

    // 2. Allocating space:
    // This is where we get a region of the screen assigned.
    // We also tell the Ui to sense clicks in the allocated region.
    let (rect, mut response) = ui.allocate_exact_size(desired_size, egui::Sense::click());

    // 3. Interact: Time to check for clicks!
    if response.clicked() {
        *on = !*on;
        response.mark_changed(); // report back that the value changed
    }

    // Attach some meta-data to the response which can be used by screen readers:
    response.widget_info(|| egui::WidgetInfo::selected(egui::WidgetType::Checkbox, *on, ""));

    // 4. Paint!
    // Make sure we need to paint:
    if ui.is_rect_visible(rect) {
        // Let's ask for a simple animation from egui.
        // egui keeps track of changes in the boolean associated with the id and
        // returns an animated value in the 0-1 range for how much "on" we are.
        let how_on = ui.ctx().animate_bool(response.id, *on);
        // We will follow the current style by asking
        // "how should something that is being interacted with be painted?".
        // This will, for instance, give us different colors when the widget is hovered or clicked.
        let visuals = ui.style().interact_selectable(&response, *on);
        // All coordinates are in absolute screen coordinates so we use `rect` to place the elements.
        let rect = rect.expand(visuals.expansion);
        let radius = 0.5 * rect.height();
        ui.painter()
            .rect(rect, radius, visuals.bg_fill, visuals.bg_stroke);
        // Paint the circle, animating it from left to right with `how_on`:
        let circle_x = egui::lerp((rect.left() + radius)..=(rect.right() - radius), how_on);
        let center = egui::pos2(circle_x, rect.center().y);
        ui.painter()
            .circle(center, 0.75 * radius, visuals.bg_fill, visuals.fg_stroke);
    }

    // All done! Return the interaction response so the user can check what happened
    // (hovered, clicked, ...) and maybe show a tooltip:
    response
}

// A wrapper that allows the more idiomatic usage pattern: `ui.add(toggle(&mut my_bool))`
/// iOS-style toggle switch.
///
/// ## Example:
/// ``` ignore
/// ui.add(toggle(&mut my_bool));
/// ```
pub(crate) fn toggle(on: &mut bool) -> impl egui::Widget + '_ {
    move |ui: &mut egui::Ui| toggle_ui(ui, on)
}

fn piece_shape(colour: hive::Colour, radius: f32, centre: egui::Pos2) -> epaint::Shape {
    let vertices = (0..6)
        .map(|i| {
            let angle = i as f32 * 2. * std::f32::consts::PI / 6.;
            centre + radius * egui::Vec2::new(angle.cos(), angle.sin())
        })
        .rev()
        .collect();
    epaint::Shape::convex_polygon(
        vertices,
        match colour {
            hive::Colour::White => egui::Color32::from_rgb(237, 216, 194),
            hive::Colour::Black => egui::Color32::from_rgb(5, 5, 5),
        },
        egui::Stroke::default(),
    )
}

fn space_shape(radius: f32, centre: egui::Pos2) -> epaint::Shape {
    let vertices = (0..6)
        .map(|i| {
            let angle = i as f32 * 2. * std::f32::consts::PI / 6.;
            centre + radius * egui::Vec2::new(angle.cos(), angle.sin())
        })
        .rev()
        .collect();
    epaint::Shape::convex_polygon(
        vertices,
        // NOTE: egui considers completely invisible shapes to have 0 size.
        egui::Color32::from_black_alpha(63),
        egui::Stroke::default(),
    )
}

fn selection_shape(radius: f32, centre: egui::Pos2, border: f32) -> epaint::Shape {
    let vertices = (0..6)
        .map(|i| {
            let angle = i as f32 * 2. * std::f32::consts::PI / 6.;
            centre + radius * egui::Vec2::new(angle.cos(), angle.sin())
        })
        .rev()
        .collect();
    epaint::Shape::convex_polygon(
        vertices,
        egui::Color32::TRANSPARENT,
        egui::Stroke::new(border, egui::Color32::GREEN),
    )
}

fn piece_icon(
    icons: &EnumMap<hive::PieceType, egui_extras::RetainedImage>,
    ctx: &egui::Context,
    typ: hive::PieceType,
    radius: f32,
    centre: egui::Pos2,
) -> epaint::Shape {
    let mut mesh = egui::Mesh::with_texture(icons[typ].texture_id(ctx));
    mesh.add_rect_with_uv(
        egui::Rect::from_center_size(centre, egui::Vec2::splat(2. * radius)),
        egui::Rect::from_min_size(egui::Pos2::ZERO, egui::Vec2::splat(1.)),
        egui::Color32::WHITE,
    );
    epaint::Shape::Mesh(mesh)
}

fn indicator(radius: f32, centre: egui::Pos2) -> epaint::Shape {
    epaint::Shape::circle_filled(centre, radius * 0.25, egui::Color32::GREEN)
}

fn hex_offset(pos: hive::Pos) -> egui::Vec2 {
    egui::Vec2::new(
        pos.x as f32 * std::f32::consts::FRAC_PI_6.cos(),
        -(pos.y as f32) - pos.x as f32 * std::f32::consts::FRAC_PI_6.sin(),
    )
}

pub(crate) fn hive_ui(
    ui: &mut egui::Ui,
    icons: &EnumMap<hive::PieceType, egui_extras::RetainedImage>,
    game: &mut hive::Game,
    selection: &mut Selection,
    engine_send: &mpsc::Sender<engine::Msg>,
) -> egui::Response {
    let radius = ui.spacing().interact_size.x;
    let border = ui.spacing().button_padding.x;
    let grid_size = 2. * radius;

    // The shapes to draw and handle clicks for, in drawing order.
    let mut shapes: Vec<_> = game
        .hive()
        .tiles()
        .map(|(pos, piece)| {
            (
                pos,
                false,
                piece_shape(
                    piece.col(),
                    radius,
                    egui::Pos2::default() + grid_size * hex_offset(pos),
                ),
                piece_icon(
                    icons,
                    ui.ctx(),
                    piece.typ(),
                    radius,
                    egui::Pos2::default() + grid_size * hex_offset(pos),
                ),
            )
        })
        .collect();

    // Add destination indicators to the end so they are drawn last.
    if !game.over() {
        match *selection {
            Selection::Nothing => {}
            Selection::InHand(piece_type) => {
                if game.current_queen().is_some()
                    || game.turn_counter() < 6
                    || piece_type == hive::PieceType::Queen
                {
                    shapes.extend(game.place_locations().into_iter().map(|pos| {
                        (
                            pos,
                            true,
                            space_shape(
                                radius,
                                egui::Pos2::default() + grid_size * hex_offset(pos),
                            ),
                            indicator(radius, egui::Pos2::default() + grid_size * hex_offset(pos)),
                        )
                    }));
                }
            }
            Selection::InHive(src) => {
                if game.current_queen().is_some() {
                    shapes.extend(game.destinations(src).into_iter().map(|pos| {
                        (
                            pos,
                            true,
                            space_shape(
                                radius,
                                egui::Pos2::default() + grid_size * hex_offset(pos),
                            ),
                            indicator(radius, egui::Pos2::default() + grid_size * hex_offset(pos)),
                        )
                    }));
                }
                shapes.push((
                    src,
                    true,
                    selection_shape(
                        radius,
                        egui::Pos2::default() + grid_size * hex_offset(src),
                        border,
                    ),
                    egui::Shape::Noop,
                ));
            }
        }
    }

    let bounding_rect = shapes
        .iter()
        .map(|(_, _, s, _)| s.visual_bounding_rect())
        .reduce(|a, b| a.union(b))
        .unwrap_or_else(|| space_shape(radius, egui::Pos2::default()).visual_bounding_rect());
    let (mut response, painter) = ui.allocate_painter(bounding_rect.size(), egui::Sense::click());

    assert!(
        bounding_rect.height() - response.rect.height() < 0.01,
        "{} < {}",
        response.rect.height(),
        bounding_rect.height()
    );
    assert!(
        bounding_rect.width() - response.rect.width() < 0.01,
        "{} < {}",
        response.rect.width(),
        bounding_rect.width()
    );

    let click_pos = if response.clicked() {
        response.interact_pointer_pos()
    } else {
        None
    };

    let painter_centre = response.rect.center();
    let hive_centre = bounding_rect.center();

    // Draw the shapes.
    for (_, _, shape, icon) in shapes.iter_mut() {
        shape.translate(painter_centre - hive_centre);
        icon.translate(painter_centre - hive_centre);
        painter.add(shape.clone());
        painter.add(icon.clone());
    }

    // Handle clicks in reverse drawing order.
    for (pos, is_destination, shape, _) in shapes.into_iter().rev() {
        if let Some(p) = click_pos {
            if shape.visual_bounding_rect().contains(p) {
                if !is_destination {
                    // A possible new src pos was clicked.
                    let piece = game.hive()[pos].last().unwrap();
                    if piece.col() == game.turn() {
                        *selection = Selection::InHive(pos);
                        response.mark_changed();
                    }
                } else {
                    // A dst pos was clicked.
                    let selected_move = match *selection {
                        Selection::Nothing => unreachable!(),
                        Selection::InHand(piece) => hive::Move::Place { piece, dst: pos },
                        Selection::InHive(src) => hive::Move::Move { src, dst: pos },
                    };
                    game.make_move(selected_move).expect("illegal move entered");
                    engine_send
                        .send(engine::Msg::MakeMove(selected_move))
                        .unwrap();
                    // Skip until a player can move or a draw is triggered.
                    // This relies on the fact that a skip is legal only when
                    // a player has no legal moves and that repetitions are detected.
                    while game.make_move(hive::Move::Skip).is_some() {
                        engine_send
                            .send(engine::Msg::MakeMove(hive::Move::Skip))
                            .unwrap();
                    }
                    *selection = Selection::Nothing;
                    response.mark_changed();
                }
                break;
            }
        }
    }

    if click_pos.is_some() && !response.changed() {
        *selection = Selection::Nothing;
        response.mark_changed();
    }

    response
}

pub(crate) fn hive<'a>(
    icons: &'a EnumMap<hive::PieceType, egui_extras::RetainedImage>,
    game: &'a mut hive::Game,
    selection: &'a mut Selection,
    engine_send: &'a mpsc::Sender<engine::Msg>,
) -> impl egui::Widget + 'a {
    move |ui: &mut egui::Ui| hive_ui(ui, icons, game, selection, engine_send)
}

pub(crate) fn hand_ui(
    ui: &mut egui::Ui,
    icons: &EnumMap<hive::PieceType, egui_extras::RetainedImage>,
    hand_colour: hive::Colour,
    hand: &hive::Hand,
    turn: hive::Colour,
    selection: &mut Selection,
) -> egui::Response {
    let radius = ui.spacing().interact_size.x;
    let border = ui.spacing().button_padding.x;
    let step = 2. * radius + ui.spacing().item_spacing.x;

    let mut shapes: Vec<_> = Vec::with_capacity(6);
    for (i, piece_type) in hand.pieces().enumerate() {
        let pos = egui::Pos2::default() + i as f32 * egui::Vec2::new(step, 0.);
        shapes.push((
            piece_type,
            piece_shape(hand_colour, radius, pos),
            piece_icon(icons, ui.ctx(), piece_type, radius, pos),
        ));
        if hand_colour == turn && *selection == Selection::InHand(piece_type) {
            shapes.push((
                piece_type,
                selection_shape(radius, pos, border),
                egui::Shape::Noop,
            ));
        }
    }
    let bounding_rect = shapes
        .iter()
        .map(|(_, s, _)| s.visual_bounding_rect())
        .reduce(|a, b| a.union(b))
        .unwrap_or_else(|| space_shape(radius, egui::Pos2::default()).visual_bounding_rect());
    let (mut response, painter) = ui.allocate_painter(bounding_rect.size(), egui::Sense::click());

    assert!(
        bounding_rect.height() - response.rect.height() < 0.01,
        "{} < {}",
        response.rect.height(),
        bounding_rect.height()
    );
    assert!(
        bounding_rect.width() - response.rect.width() < 0.01,
        "{} < {}",
        response.rect.width(),
        bounding_rect.width()
    );

    let click_pos = if response.clicked() {
        response.interact_pointer_pos()
    } else {
        None
    };

    let painter_centre = response.rect.center();
    let hand_centre = bounding_rect.center();
    for (piece_type, mut shape, mut icon) in shapes {
        shape.translate(painter_centre - hand_centre);
        icon.translate(painter_centre - hand_centre);
        if let Some(p) = click_pos {
            if hand_colour == turn && shape.visual_bounding_rect().contains(p) {
                *selection = Selection::InHand(piece_type);
                response.mark_changed();
            }
        }
        painter.add(shape);
        painter.add(icon);
    }

    if click_pos.is_some() && !response.changed() {
        *selection = Selection::Nothing;
        response.mark_changed();
    }

    response
}

pub(crate) fn hand<'a>(
    icons: &'a EnumMap<hive::PieceType, egui_extras::RetainedImage>,
    hand_colour: hive::Colour,
    hand: &'a hive::Hand,
    turn: hive::Colour,
    selection: &'a mut Selection,
) -> impl egui::Widget + 'a {
    move |ui: &mut egui::Ui| hand_ui(ui, icons, hand_colour, hand, turn, selection)
}
