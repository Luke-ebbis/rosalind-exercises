// Usefull documentation https://stackoverflow.com/questions/41301239/how-to-unbox-elements-contained-in-polymorphic-vectors


/// # Biological files.
pub mod bio {
    use crate::lib::sequence::{Dna, Length, Rna};
    use crate::lib::sequence::strings::{Alphabets, Sequence, SequenceError};

    /// # The Bio sequences for which there is an implementation.
    #[derive(Clone)]
    pub enum Biosequence {
        /// An Rna sequence.
        DnaType(Dna),
        /// An Dna sequence.
        RnaType(Rna),
    }

    impl Biosequence {
        pub fn new(input: impl  Into<String>, alphabet: Alphabets) -> Result<Biosequence, SequenceError> {
            let input = input.into();
            match alphabet {
                Alphabets::Dna => Ok(Self::DnaType(Dna::new(input)?)),
                Alphabets::Rna => Ok(Self::RnaType(Rna::new(input)?)),
                _ => unimplemented!()
            }
        }

        /// # Convert to a general Sequence object.
        /// Warning: when you do this, you loose all information regarding underlying class. It may be used to accessed
        /// the universal sized properties of a sequence: [crate::lib::sequence::Length] and
        /// [crate::lib::sequence::Frequency].
        pub fn to_sequence(self) -> Box<dyn crate::lib::sequence::Sequence> {
            match self {
                Biosequence::DnaType(dna) => Box::new(dna),
                Biosequence::RnaType(rna) => Box::new(rna),
            }
        }
    }

    pub struct FastaRecord {
        /// This struct implements all traits from [crate::lib::sequence::Sequence]. If you want more specific methods
        /// that are implemented by [Biosequence], then you should match on this.
        sequence: Biosequence,

        identifier: String,
        /// Optional description.
        description: Option<String>,
    }

    impl FastaRecord {
        pub fn new(input: Biosequence, identifier: impl Into<String>, description: Option<String>) -> Result<FastaRecord, SequenceError> {
            Ok(Self {sequence: input, identifier: identifier.into(), description: description})
        }
    }
}
mod test {
    use std::thread::yield_now;
    use crate::lib::io::bio::{Biosequence, FastaRecord};
    use crate::lib::io::bio::Biosequence::DnaType;
    use crate::lib::sequence::{Complement, Frequency, Length, Dna, Rna};
    use crate::lib::sequence::strings::Alphabets::Dna as DnaNucleotides;

    #[test]
    fn types_test() {
        let x = Biosequence::new("AATTT",  DnaNucleotides).unwrap();
        let y = x.clone().to_sequence();
        let len = y.length();
        let seq= y.get();
        let com = y.frequency();
        match x {
            DnaType(dna) => {let t =dna.complement().complement();
                let r: Rna = t.to_owned().into();
                t},
            _ => panic!(),
        };
    }

    #[test]
    fn fasta_test() {
        let dna = FastaRecord::new(Biosequence::new("AAA", DnaNucleotides).unwrap(), "test one", None);
    }
}
