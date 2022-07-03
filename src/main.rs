use crate::bot::*;
use crate::game::*;
use rand::{thread_rng, Rng};
use std::io;
mod bot;
mod game;
mod hive;
mod render;
mod small_arrayvec;

fn main() -> io::Result<()> {
    let mut game = Game::new();
    let mut bot = Bot::new(2_000_000);
    let mut rng = thread_rng();
    println!("{}", game);

    // Agent vs agent, with each agent either choosing the best move or a random move.
    // while !game.over() {
    //     if rng.gen() {
    //         bot.make_best_move(&mut game, 5);
    //     } else {
    //         let moves = game.moves();
    //         game.make_move(game.offset_move(moves[rng.gen_range(0..moves.len())]));
    //     }
    //     println!("\n{}", game);
    // }

    // Agent vs human
    loop {
        bot.make_best_move(&mut game, 5);
        println!("\n{}", game);
        if game.over() {
            break;
        }
        let mut buffer = String::new();
        loop {
            io::stdin().read_line(&mut buffer)?;
            match buffer.parse::<AbsMove>() {
                Ok(m) => {
                    if game.moves().contains(&game.unoffset_move(m)) {
                        game.make_move(m);
                        break;
                    }
                    println!("Illegal move");
                }
                Err(e) => {
                    println!("Invalid syntax: {}", e);
                }
            }
            buffer.clear();
        }
        println!("\n{}", game);
        if game.over() {
            break;
        }
    }
    Ok(())
}
