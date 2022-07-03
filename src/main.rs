use crate::bot::*;
use crate::game::*;
use rand::prelude::*;
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
    //     let m = if rng.gen() {
    //         bot.best_move(&mut game, 5)
    //     } else {
    //         let moves = game.moves();
    //         *moves.choose(&mut rng).unwrap()
    //     };
    //     game.make_move(m);
    //     println!("{}\n{}", m, game);
    // }

    // Agent vs human
    loop {
        let m = bot.best_move(&mut game, 6);
        game.make_move(m);
        println!("{}\n{}", m, game);
        if game.over() {
            break;
        }
        let mut buffer = String::new();
        loop {
            io::stdin().read_line(&mut buffer)?;
            match buffer.parse::<AbsMove>() {
                Ok(m) => {
                    if game.moves().contains(&m) {
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
        println!("{}", game);
        if game.over() {
            break;
        }
    }
    Ok(())
}
