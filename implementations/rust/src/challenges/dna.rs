use std::fmt;
use crate::lib::sequence::strings::SequenceError;
use crate::lib::sequence::Dna;
use crate::lib::sequence::Frequency;
pub struct DnaCount {
    pub a: i32,
    pub t: i32,
    pub c: i32,
    pub g: i32,
}
impl fmt::Display for DnaCount {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{} {} {} {}", self.a, self.c, self.g, self.t)
    }
}
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

    #[test]
    fn example_data() {
        let input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC".to_string();
        let dna_count = dna_count(input).unwrap();
        print!("{dna_count}");
    }
}
