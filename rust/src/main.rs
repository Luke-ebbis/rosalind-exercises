use rust::sequence;

use crate::Challenges::Dna;
use clap::{Error, Parser};

/// My programme for the rosalind work in `rust`.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Challenge to complete.
    #[arg(short, long)]
    challenge: String,
}
#[derive(Debug)]
pub enum Challenges {
    Dna,
}

impl Challenges {
    fn new(string: &str) -> Challenges {
        match string {
            "dna" => Dna,
            _ => unimplemented!(
                "This challenge: {} is not yet implemented!",
                string
            ),
        }
    }
}

fn main() {
    let args = Args::parse();
    let challenge = Challenges::new(&args.challenge);
    dbg!("Challenge: {}", challenge);
}
