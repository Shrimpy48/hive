use hive::*;
use std::io;
use std::time::{Duration, Instant};

fn main() -> io::Result<()> {
    let mut game = Game::new();
    let mut mini = bot::minimax::Bot::new(2_000_000);
    let mut monty = bot::mcts::Bot::new();
    println!("{}", game);

    // Agent vs agent, with each agent either choosing the best move or a random move.
    loop {
        let m = mini.best_move(&mut game, 5);
        game.make_move_unchecked(m);
        monty.discard_others(m);
        println!("{}\n{}", m, game);

        if game.over() {
            break;
        }

        let m = monty.mcts(Instant::now() + Duration::from_secs(10));
        game.make_move_unchecked(m);
        monty.discard_others(m);
        println!("{}\n{}", m, game);

        if game.over() {
            break;
        }
    }

    // Agent vs human
    // loop {
    //     let m = bot.best_move(&mut game, 6);
    //     game.make_move(m);
    //     println!("{}\n{}", m, game);
    //     if game.over() {
    //         break;
    //     }
    //     let mut buffer = String::new();
    //     loop {
    //         io::stdin().read_line(&mut buffer)?;
    //         match buffer.parse::<AbsMove>() {
    //             Ok(m) => {
    //                 if game.make_move(m).is_some() {
    //                     break;
    //                 }
    //                 println!("Illegal move");
    //             }
    //             Err(e) => {
    //                 println!("Invalid syntax: {}", e);
    //             }
    //         }
    //         buffer.clear();
    //     }
    //     println!("{}", game);
    //     if game.over() {
    //         break;
    //     }
    // }
    Ok(())
}
