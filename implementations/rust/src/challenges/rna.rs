//! # Solution to `rna`.

use crate::lib::sequence::strings::SequenceError;
use crate::lib::sequence::{Dna, Rna};

/// # Transcribe a string of (presumed) DNA to RNA.
/// If the input string is not DNA, and error is returned.
pub fn transcribe_rna(string: String) -> Result<Rna, SequenceError> {
    let seq = Dna::new(string.to_uppercase());
    match seq {
        Ok(sequence) => Ok(sequence.transcribe()),
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod test {
    use crate::challenges::rna::transcribe_rna;
    #[test]
    fn example_data() {
        let input = "GATGGAACTTGACTACGTAAATT".to_string();
        let rna = transcribe_rna(input).unwrap();
        assert_eq!(format!("{rna}"), "GAUGGAACUUGACUACGUAAAUU".to_string());
    }
}
