//! # Solution to `dna`.
use crate::lib::sequence::strings::SequenceError;
use crate::lib::sequence::Dna;
use crate::lib::sequence::Frequency;
use std::fmt;

/// # Simple DNA count Statistics.
/// A simple struct to store amounts of `a`, `t`, `c` and `g`.
#[derive(Debug, Clone)]
pub struct DnaCount {
    pub a: i32,
    pub t: i32,
    pub c: i32,
    pub g: i32,
}

/// # Displaying A DnaCount.
///
/// Order of printing is `<A>, <C>, <G>, <T>`.
impl fmt::Display for DnaCount {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{} {} {} {}", self.a, self.c, self.g, self.t)
    }
}

/// # Counting the frequencies of nucleotides in A string that might be DNA.
///
/// The input is a string that can be Dna, if it is not Dna, an sequence error is returned.
pub fn dna_count(string: String) -> Result<DnaCount, SequenceError> {
    let seq = Dna::new(string.to_lowercase());
    match seq {
        Ok(sequence) => {
            let freq = sequence.frequency();
            let result = DnaCount {
                a: *freq.get(&'a').unwrap(),
                t: *freq.get(&'t').unwrap(),
                c: *freq.get(&'c').unwrap(),
                g: *freq.get(&'g').unwrap(),
            };
            Ok(result)
        }
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod test {
    use crate::challenges::dna::*;
    use crate::Challenges::Dna;

    #[test]
    fn example_data() {
        let input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC".to_string();
        let expected = "20 12 17 21".to_string();
        let dna_count = dna_count(input).unwrap();
        let result = format!("{dna_count}");
        assert_eq!(expected, result);
    }

    #[test]
    fn zero_count() {
        let input = "AAATT".to_string();
        let expected = "3 0 0 2".to_string();
        let dna_count = dna_count(input).unwrap();
        let result = format!("{dna_count}");
        assert_eq!(expected, result);
    }
}
