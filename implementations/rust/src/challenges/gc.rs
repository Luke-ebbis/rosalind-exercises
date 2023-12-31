//! # Solution to the `gc` challenge
//! Open an `fasta` file, determine which record has the highest `GC`%.

use crate::lib::io::bio::{FastaReadError};

use crate::lib::sequence::GcFraction;

use std::fs::File;
use std::io::Error;

/// What can go wrong when analysing the cg content of a fasta file.
#[derive(Debug)]
pub enum GcError {
    /// When the file cannot be opened.
    FileError(Error),
    /// When there is an issue during opening of the FastaFile.
    SequenceError(FastaReadError),
    /// When the Fasta file does not contain Dna.
    NoDnaError,
}

/// # Determine which DNA sequence in a [fasta] file has the highest GC content.
/// If the gc content can be calculated without getting a [GcError], the resulting [String] will be of the following
/// format:
/// ```text
/// Identifier
/// <cg>
/// ```
pub fn highest_cg_from_fasta(
    fasta_file: impl Into<String>
) -> Result<String, GcError> {
    let file = match File::open(fasta_file.into()) {
        Ok(file) => file,
        Err(e) => return Err(GcError::FileError(e)),
    };
    let fasta = match crate::lib::io::bio::FastaRecords::new(file) {
        Ok(fa) => fa,
        Err(e) => return Err(GcError::SequenceError(e)),
    };
    let mut keys = fasta.keys();
    let mut max_sequence = fasta.get(keys.pop().unwrap());
    let mut max_gc = match max_sequence.get_sequence() {
        crate::lib::io::bio::Biosequence::DnaType(dna) => dna.gc(),
        _ => return Err(GcError::NoDnaError),
    };
    for key in keys {
        let sequence = fasta.get(key);
        let gc = match sequence.get_sequence() {
            crate::lib::io::bio::Biosequence::DnaType(dna) => dna.gc(),
            _ => return Err(GcError::NoDnaError),
        };
        if gc >= max_gc {
            max_gc = gc;
            max_sequence = sequence;
        }
    }
    let output = format!("{}\n{:.6}", max_sequence.identifier, max_gc*100.0);
    Ok(output)
}

#[cfg(test)]
mod test {
    use crate::challenges::gc::highest_cg_from_fasta;

    /// Run the test dataset from Rosalind.
    #[test]
    fn cg() {
        let gc = highest_cg_from_fasta("data/gc-test.txt").unwrap();
        let expected = "Rosalind_0808\n60.919540".to_string();
        assert_eq!(gc, expected);
    }
}
