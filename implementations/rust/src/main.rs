//! My work on rosalind in `rust`.

use clap::Parser;
use std::{fmt, fs};
mod challenges;
mod lib;

/// My programme for the rosalind work in `rust`.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Challenge to complete.
    #[arg(name = "challenge")]
    challenge: String,

    #[arg(name = "input data")]
    input_file: String,
}

/// # The Challenges that have been implemented thus far.
#[derive(Debug)]
pub enum Challenges {
    /// The DNA counting challenge using [challenges::dna].
    Dna,
    /// The RNA counting challenge using [challenges::rna].
    Rna,
    /// Reverse complement Dna [challenges::revc].
    Revc,
}

impl Challenges {
    const IMPLEMENTED: [&'static Challenges; 3] = [
        &crate::Challenges::Dna,
        &crate::Challenges::Rna,
        &crate::Challenges::Revc,
    ];
    fn new(
        string: &str,
        args: &Args,
    ) -> () {
        match string {
            "dna" => {
                let input = fs::read_to_string(args.input_file.clone())
                    .expect("Should have been able to read the file")
                    .replace("\n", "");
                let count = challenges::dna::dna_count(input).unwrap();
                println!("{count}")
            }
            "rna" => {
                let input = fs::read_to_string(args.input_file.clone())
                    .expect("Should have been able to read the file")
                    .replace("\n", "");
                let rna = challenges::rna::transcribe_rna(input).unwrap();
                println!("{rna}")
            }
            "revc" => {
                let input = fs::read_to_string(args.input_file.clone())
                    .expect("Should have been able to read the file")
                    .replace("\n", "");
                let rna =
                    challenges::revc::dna_reverse_complement(input).unwrap();
                println!("{rna}")
            }
            _ => unimplemented!(
                "Supplied challenge string: `{}' is not yet implemented!\n\
            the challenges {:?} can be chosen.",
                string,
                Challenges::IMPLEMENTED
            ),
        }
    }
}

impl fmt::Display for Challenges {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let repr = match self {
            Challenges::Dna => "dna".to_string(),
            Challenges::Rna => "rna".to_string(),
            Challenges::Revc => "revc".to_string(),
        };
        write!(f, "{}", repr)
    }
}

/// Running the challenges.
fn main() {
    let args = Args::parse();
    let _challenge = Challenges::new(&args.challenge, &args);
}
