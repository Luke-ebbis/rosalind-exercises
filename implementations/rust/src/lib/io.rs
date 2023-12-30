//! # Input and output of files

// Usefull documentation https://stackoverflow.com/questions/41301239/how-to-unbox-elements-contained-in-polymorphic-vectors


/// # Biological files.
/// Currently, [FastaRecords] are implemented.
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

    /// # A container to hold fasta record data
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

        pub fn get_sequence(&self) -> &Biosequence {
            &self.sequence
        }
    }

    // TODO multiple error handling by enumerating possible error conditions: https://stackoverflow.com/questions/71812362/rust-error-handling-capturing-multiple-errors


    /// # The possible error states of the [FastaRecords] struct.
    #[derive(Debug)]
    pub enum FastaReadError {
        /// When the file contains characters that cannot be understood by `rust`.
        FileIoError(Error),
        /// When the sequence contains letters that do not fit the [Alphabet].
        SequenceContentError(SequenceError)
    }

    #[derive(Debug)]
    pub struct FastaRecords {
        records: HashMap<String, FastaRecord>
    }


    impl FastaRecords {
        /// # Parse a fasta file object into a Struct of fasta records.
        pub fn new(f: File) -> Result<FastaRecords, FastaReadError> {
            let mut fastas: HashMap<String, FastaRecord> = HashMap::new();
            let reader = BufReader::new(f);
            let mut seq: Vec<String> = Vec::new();
            let mut header : (String, Option<String>) = ("".to_string(), None);
            for line in reader.lines() {
                let current_line = match line {
                    Ok(line) => line,
                    Err(e) => return Err(FastaReadError::FileIoError(e)),
                };
                if current_line.starts_with(">") {
                    let header = Self::metadata(&current_line);
                    match FastaRecords::insert_set(&mut fastas, seq.join(""), header.clone()) {
                        Ok(_) => {},
                        Err(e) => {return Err(FastaReadError::SequenceContentError(e))},
                    };
                } else {
                    &seq.push(current_line.to_string().replace("\n", " "));
                }
            }
            match FastaRecords::insert_set(&mut fastas, seq.join(""), header.clone()) {
                Ok(_) => {},
                Err(e) => {return Err(FastaReadError::SequenceContentError(e))},
            };
            Ok(Self {records: fastas})
        }

        /// # Return the number of fasta records within the container
        pub fn len(&self) -> usize {
            self.records.keys().len().to_owned()
        }


        /// # Return the id values of each fasta sequence in the set.
        pub fn keys(&self) -> Vec<String> {
            self.records.keys().into_iter().map(|x| x.to_owned()).collect()
        }

        pub fn get(&self, id: impl Into<String>) -> &FastaRecord {
            let id = id.into();
            let fasta = self.records.get(&id).unwrap();
            fasta.clone()
        }

        /// # Insert a DNA sequence.
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
    use crate::lib::sequence::{Complement, Frequency, Length, Dna, Rna, GcFraction};
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
        assert_eq!(count, 5 as usize);
        let id = fasta.keys();
        let fasta_rec = fasta.get(id.get(1).unwrap());
        let seq1 = match fasta_rec.get_sequence() {
            DnaType(dna) => {
                let len = dna.to_owned().length();
                dbg!(len);
                let x = dna.to_owned().gc();
                println!("{}", x)},
            _ => panic!()
        };
    }

    #[test]
    fn false_fasta() {
        let file = File::open("data/false_fasta.txt").unwrap();
        let fasta = FastaRecords::new(file).unwrap();
        let id = fasta.keys();
        println!("{:?}", id);
    }
}
