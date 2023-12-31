//! # Solution to `Revc`.
use crate::lib::sequence::strings::SequenceError;

use crate::lib::sequence::{Complement, Dna, Reverse, Sequence};


/// # Get the reverse complement of an input (Dna?) String.
/// If the string is not Dna, a sequence error is returned.
pub fn dna_reverse_complement(input: String) -> Result<String, SequenceError> {
    let result = Dna::new(input)?.reverse().complement().get().to_string();
    Ok(result)
}

#[cfg(test)]
mod test {
    use crate::lib::sequence::{Complement, Dna, Reverse, Sequence};

    #[test]
    fn example_data() {
        let input = "AAAACCCGGT".to_string();
        let expected = "ACCGGGTTTT".to_string();
        let result = Dna::new(input)
            .unwrap()
            .reverse()
            .complement()
            .get()
            .to_string();
        assert_eq!(expected, result);
    }
}
