use rust::sequence;
use std::{fmt, fs};

use crate::Challenges::Dna;
use clap::{Error, Parser};
use rust::dna;
use rust::dna::{dna_count, DnaCount};

/// My programme for the rosalind work in `rust`.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Challenge to complete.
    #[arg(name="challenge")]
    challenge: String,

    #[arg(short, long)]
    input_file: String,
}
#[derive(Debug)]
pub enum Challenges {
    /// The DNA counting challenge.
    Dna,
}

impl Challenges {
    const IMPLEMENTED:  [&'static Challenges; 1] = [&crate::Challenges::Dna];
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
            _ => unimplemented!("Supplied challenge string: `{}' is not yet implemented!\n\
            the challenges {:?} can be chosen.", string, Challenges::IMPLEMENTED),
        }
    }
}

impl fmt::Display for Challenges {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let repr = match self {
            Dna => "Dna".to_string(),
        };
        write!(f, "{}", repr)
    }
}
fn main() {
    let args = Args::parse();
    let challenge = Challenges::new(&args.challenge, &args);
}
