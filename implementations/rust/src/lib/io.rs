// Usefull documentation https://stackoverflow.com/questions/41301239/how-to-unbox-elements-contained-in-polymorphic-vectors


/// # Biological files.
pub mod bio {
    use std::collections::{HashMap, HashSet};
    use std::fmt;
    use std::fs::{File, metadata};
    use std::io::{BufRead, BufReader, Error};
    use clap::builder::Str;
    use crate::lib::sequence::{Dna, Length, Rna};
    use crate::lib::sequence::strings::{Alphabet, Alphabets, Sequence, SequenceError};

    /// # The Bio sequences for which there is an implementation.
    #[derive(Clone, Debug)]
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

    #[derive(Debug)]
    pub struct FastaRecord {
        /// This struct implements all traits from [crate::lib::sequence::Sequence]. If you want more specific methods
        /// that are implemented by [Biosequence], then you should match on this.
        sequence: Biosequence,
        // Unique identifier.
        identifier: String,
        /// Optional description.
        description: Option<String>,
    }

    impl FastaRecord {
        pub fn new(input: Biosequence, identifier: impl Into<String>, description: Option<String>) -> Result<FastaRecord, SequenceError> {
            Ok(Self {sequence: input, identifier: identifier.into(), description: description})
        }
    }

    // #[derive(Debug, PartialEq, Clone)]
    // pub struct FastaParseError {
    //     /// What the error is about.
    //     description: String,
    // }
    //
    // impl FastaParseError {
    //     pub fn new(
    //         description: String
    //     ) -> FastaParseError {
    //         Self {
    //             description: description,
    //         }
    //     }
    // }
    //
    // impl fmt::Display for FastaParseError {
    //     fn fmt(
    //         &self,
    //         f: &mut fmt::Formatter,
    //     ) -> fmt::Result {
    //         write!(
    //             f,
    //             "{:?}\n",
    //             self.description
    //         )
    //     }
    // }
    // TODO multiple error handling by enumerating possible error conditions: https://stackoverflow.com/questions/71812362/rust-error-handling-capturing-multiple-errors


    #[derive(Debug)]
    pub struct FastaRecords {
        records: HashMap<String, FastaRecord>
    }

    impl FastaRecords {
        /// # Parse a fasta file object into a Struct of fasta records.
        pub fn new(f: File) -> Result<FastaRecords, Error> {
            let mut fastas: HashMap<String, FastaRecord> = HashMap::new();
            let reader = BufReader::new(f);
            let mut seq: Vec<String> = Vec::new();
            let mut header : (String, Option<String>) = ("".to_string(), None);
            for line in reader.lines() {
                let current_line = line?;
                if current_line.starts_with(">") {
                    println!("{:?}", seq.clone());
                    println!("**in name region");
                    let header = Self::metadata(&current_line);
                    println!("{:?}", header);
                    FastaRecords::insert_set(&mut fastas, seq.join(""), header.clone());
                    // fastas.insert(
                    //     header.0.clone(),
                    //     FastaRecord::new(Biosequence::DnaType(Dna::new(seq.join("")).unwrap()), header.0, header.1).unwrap(),
                    // );
                    let mut seq: Vec<String> = Vec::new();
                } else {
                    &seq.push(current_line.to_string().replace("\n", " "));
                }
            }
            FastaRecords::insert_set(&mut fastas, seq.join(""), header.clone());
            // fastas.insert(
            //     header.0.clone(),
            //     FastaRecord::new(Biosequence::DnaType(Dna::new(seq.join("")).unwrap()), header.0, header.1).unwrap(),
            // );
            Ok(Self {records: fastas})
        }

        /// # Return the number of fasta records within the container
        pub fn len(&self) -> usize {
            self.records.keys().len().to_owned()
        }

        pub fn keys(&self) -> Vec<String> {
            self.records.keys().into_iter().map(|x| x.to_owned()).collect()
        }

        fn insert_set(set: &mut HashMap<String, FastaRecord>,
                      dna: impl  Into<String>, headers: (String, Option<String>)) -> Result<(), SequenceError> {
            let dna = dna.into();
            let input_dna = Dna::new(dna)?;
            if headers.0.clone() != "" {
                set.insert(headers.0.clone(),
                           FastaRecord::new(Biosequence::DnaType(input_dna),
                                            headers.0,
                                            headers.1)?);
            };
            Ok(())
        }

        /// # Extract the metadata from the header line.
        fn metadata(current_line: &String) -> (String, Option<String>) {
            let binding = current_line.replace(">", "");
            let header: Vec<&str> = binding.split(" ").into_iter().collect();
            let name = header.get(0).unwrap().to_owned().to_string();
            // resolving the description to an option.
            let description= match header[1..].join(" ").as_str() {
                "" => None,
                a => Some(a.to_string()),
            };
            (name, description)
        }
    }
}

#[cfg(test)]
mod test {
    use std::fs::File;
    use crate::lib::io::bio::{Biosequence, FastaRecord, FastaRecords};
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

    #[test]
    fn test_fasta_intake() {
        let file = File::open("data/rosalind_gc.txt").unwrap();
        let fasta = FastaRecords::new(file).unwrap();
        let count = fasta.len();
        dbg!(fasta.keys());
        assert_eq!(count, 5 as usize);

    }
}
