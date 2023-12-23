use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filepath = args.get(1).expect("no file path given");
    let read = parse_fasta(&Path::new(filepath));
    let seq = read.unwrap();
    let rna = translate(seq);
}

#[derive(Debug)]
struct Sequence {
    name: String,
    sequence: String,
}

/// Break the DNA sequence into a list of ORFs.
fn orfs() {
    let start: HashSet<&str> = HashSet::from(["AUG"]);
    let stop: HashSet<&str> = HashSet::from(["UAG", "UGA", "UAA"]);
}



fn translate(seq: Sequence) -> Sequence {
    let mut translated = Vec::new();
    for character in seq.sequence.chars() {
        let rna_char = match character {
            'A' => Ok('U'),
            'T' => Ok('T'),
            'A' => Ok('U'),
            'A' => Ok('U'),
            _ => Err("The provided string is not RNA!")
        }.unwrap();
        translated.push(rna_char);
        dbg!(rna_char);
    };
    seq
}

impl Sequence {
    fn new(
        name: String,
        sequence: String,
    ) -> Sequence {
        Sequence { name, sequence }
    }
}

/// Read in a fasta sequence from a file.
fn parse_fasta(filename: &Path) -> Result<Sequence, String> {
    let r = fs::read_to_string(filename).expect("The FASTA could not be read");
    let mut list: Vec<&str> = r.split("\n").collect();
    let id = list.remove(0);
    let sequence = list.join("");
    Ok(Sequence::new(id.to_string(), sequence))
}
