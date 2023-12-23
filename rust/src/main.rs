use rust::sequence;
use std::fs;

use crate::Challenges::Dna;
use clap::{Error, Parser};
use rust::dna;
use rust::dna::dna_count;

/// My programme for the rosalind work in `rust`.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Challenge to complete.
    #[arg(short, long)]
    challenge: String,

    #[arg(short, long)]
    input_file: String,
}
#[derive(Debug)]
pub enum Challenges {
    Dna,
}

impl Challenges {
    fn new(
        string: &str,
        args: &Args,
    ) -> () {
        match string {
            "dna" => {
                let input = fs::read_to_string(args.input_file.clone())
                    .expect("Should have been able to read the file")
                    .replace("\n", "");
                let count = dna_count(input).unwrap();
                println!("{count}")
            }
            _ => unimplemented!(),
        }
    }
}

fn main() {
    let args = Args::parse();
    let challenge = Challenges::new(&args.challenge, &args);
}
