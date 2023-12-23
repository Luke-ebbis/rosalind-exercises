use std::env;
// The issue with this programme is that it is not able to check whether a DNA
// sequence is valid. Maybe the next step is to reduce the error checking, and
// make the classes function. Afterwards, I can introduce error checking.
fn main() { 
    // Make sure that the user specifies a file name
    println!("Running a nice test with rust");
    let _file = env::args().skip(1).next().expect("Filename not supplied");
    let dna = dna_sequences::Dna::new(
        String::from("a"),
        String::from("AABBCD"),
        String::from("A nice comment"),
    );
    println!("The dna is {dna}");
}

// fn main() -> std::io::Result<()> {
//     // Obtaining the command line args:
//     let args: Vec<String> = env::args().collect();
//     let filename = &args[1];

//     for line in read_lines::BufReader::open(filename)? {
//         println!("{}", line?.trim());
//     }

//     Ok(())
// }

/// This module provides structs, functions, and traits related to DNA sequences.
mod dna_sequences {
    use std::fmt;

    /// A struct representing a DNA sequence, with an associated
    /// implementation. For Dna, namely checking whether the input is valid,
    /// and calculating cg percentage.
    pub struct Dna {
        identifier: String,
        sequence: String,
        comment: String,
    }
    pub struct DnaResult(Result<Dna, String>);

    impl Dna {
        /// Constructs a new `Dna` sequence struct.
        ///
        /// # Arguments
        ///
        /// * `identifier` - A `String` that identifies the sequence.
        /// * `sequence` - A `String` representing the DNA sequence.
        /// * `comment` - An optional `String` containing a comment or description of the sequence.
        ///
        /// # Returns
        ///
        /// A new `Dna` sequence struct with the specified identifier, sequence, and comment.
        pub fn new(
            identifier: String,
            sequence: String,
            comment: String,
        ) -> DnaResult {
            Self::is_valid_sequence(&sequence)?;
            Ok(Self {
                identifier,
                sequence,
                comment,
            })
        }

        /// Determines if a given DNA sequence is valid, i.e., contains only DNA nucleotides.
        ///
        /// # Arguments
        ///
        /// * `sequence` - A reference to a `str` representing the DNA sequence to be validated.
        ///
        /// # Returns
        ///
        /// `oke` if the sequence contains only valid DNA nucleotides (A, C, G,
        /// T), `error` otherwise.
        fn is_valid_sequence(sequence: &str) -> Result<(), String> {
            println!("{:?}", sequence.chars());
            let validity = sequence.chars().all(|c| c.is_simple_iupac_dna());
            if validity == true {
                Ok(())
            } else {
                Err("This is not a valid Dna sequence".to_string())
            }
        }
    }



impl fmt::Display for DnaResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Ok(dna) => write!(f, "DNA sequence {} with identifier {} and comment {}",
                              dna.sequence, dna.identifier, dna.comment),
            Err(e) => write!(f, "Error: {}", e),
        }
    }
}


    // /// To string implementation for a Dna sequence
    // impl fmt::Display for Dna {
    //     fn fmt(
    //         &self,
    //         f: &mut fmt::Formatter<'_>,
    //     ) -> fmt::Result {
    //     let output = format!("{} {}\n{}", &self.identifier,
    //         &self.sequence, &self.comment);
    //     write!(f, "{}", output)}
    // }

    /// A trait to determine if a character is a valid DNA nucleotide.
    trait IsDna {
        /// Determines if a character is a valid DNA nucleotide, i.e., 'A', 'C', 'G', or 'T'.
        ///
        /// # Returns
        ///
        /// `true` if the character is a valid DNA nucleotide, `false` otherwise.
        fn is_simple_iupac_dna(&self) -> bool;
    }

    impl IsDna for char {
        /// Determines if a character is a valid DNA nucleotide, i.e., 'A', 'C', 'G', or 'T'.
        ///
        /// # Returns
        ///
        /// `true` if the character is a valid DNA nucleotide, `false` otherwise.
        fn is_simple_iupac_dna(&self) -> bool {
            println!("{self}");
            match self.to_ascii_uppercase() {
                'A' | 'C' | 'G' | 'T' => true,
                _ => false,
            }
        }
    }
}

mod read_lines {
    // Took this code from
    // https://stackoverflow.com/questions/45882329/read-large-files-line-by-line-in-rust
    // I should re-write it myself!
    use std::{
        fs::File,
        io::{self, prelude::*},
        rc::Rc,
    };

    pub struct BufReader {
        reader: io::BufReader<File>,
        buf: Rc<String>,
    }

    fn new_buf() -> Rc<String> {
        Rc::new(String::with_capacity(1024)) // Tweakable capacity
    }

    impl BufReader {
        pub fn open(path: impl AsRef<std::path::Path>) -> io::Result<Self> {
            let file = File::open(path)?;
            let reader = io::BufReader::new(file);
            let buf = new_buf();

            Ok(Self { reader, buf })
        }
    }

    impl Iterator for BufReader {
        type Item = io::Result<Rc<String>>;

        fn next(&mut self) -> Option<Self::Item> {
            let buf = match Rc::get_mut(&mut self.buf) {
                Some(buf) => {
                    buf.clear();
                    buf
                }
                None => {
                    self.buf = new_buf();
                    Rc::make_mut(&mut self.buf)
                }
            };

            self.reader
                .read_line(buf)
                .map(|u| {
                    if u == 0 {
                        None
                    } else {
                        Some(Rc::clone(&self.buf))
                    }
                })
                .transpose()
        }
    }
}
