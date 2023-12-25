// Usefull documentation https://stackoverflow.com/questions/41301239/how-to-unbox-elements-contained-in-polymorphic-vectors


/// # Biological files.
pub mod bio {
    use crate::lib::sequence::{Dna, Length, Rna};
    use crate::lib::sequence::strings::{Sequence, SequenceError};

    /// # The Bio sequences for which there is an implementation.
    #[derive(Clone)]
    pub enum Biosequence {
        /// An Rna sequence.
        DnaType(Dna),
        /// An Dna sequence.
        RnaType(Rna),
    }

    impl Biosequence {
        pub fn new(input: String) -> Biosequence {
            Self::DnaType(Dna::new(input).unwrap())
        }

        /// # Convert to a general Sequence object.
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
        sequence: Biosequence
    }

    impl FastaRecord {
        fn new_rna(input: String) -> Result<FastaRecord, SequenceError> {
            let rna = Rna::new(input)?;
            Ok(Self {sequence: Biosequence::RnaType(rna)})
        }
        fn new_dna(input: String) -> Result<FastaRecord, SequenceError> {
            let dna= Dna::new(input)?;
            Ok(Self {sequence: Biosequence::DnaType(dna)})
        }

    }
}
mod test {
    use std::thread::yield_now;
    use crate::lib::io::bio::Biosequence;
    use crate::lib::io::bio::Biosequence::DnaType;
    use crate::lib::sequence::{Complement, Dna, Frequency, Length, Reverse};

    #[test]
    fn types_test() {
        let x = Biosequence::new("AATTT".into());
        let y = x.clone().to_sequence();
        // let k: Dna = y.into();
        let len = y.length();
        let seq= y.get();
        let com = y.frequency();
        match x {
            DnaType(dna) => dna.complement().complement(),
            _ => panic!(),
        };
    }
}
