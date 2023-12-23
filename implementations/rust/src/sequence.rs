use crate::dna::DnaCount;
use crate::sequence;
use crate::sequence::strings::Alphabets::Dna as DnaAlphabet;
use crate::sequence::strings::SequenceError;
use crate::sequence::strings::Sequences::Dna as DnaSequenceFactory;
use crate::sequence::strings::Sequences::Rna as RnaSequenceFactory;
use crate::sequence::Sequence as SequenceType;
use clap::builder::Str;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter, write};

pub trait Sequence: fmt::Display {
    fn get(&self) -> &str;
}


pub trait Length {
    fn length(&self) -> i32;
}

pub trait Frequency {
    fn frequency(&self) -> HashMap<char, i32>;
}
impl<T: Sequence> Frequency for T {
    /// # Determine the frequency of a sequence.
    ///
    /// # Returns
    /// The length of the sequence.
    fn frequency(&self) -> HashMap<char, i32> {
        let seq = self.get();
        let mut letter_counts: HashMap<char, i32> = HashMap::new();
        let char_vec: Vec<char> = seq.chars().collect();
        for c in char_vec {
            *letter_counts.entry(c).or_insert(0) += 1;
        }
        letter_counts
    }
}

impl<T: Sequence> Length for T {
    /// # Determine the length of a sequence.
    ///
    /// # Returns
    /// The length of the sequence.
    fn length(&self) -> i32 {
        let seq = self.get();
        seq.len() as i32
    }
}

pub struct Dna {
    sequence: strings::Sequence,
}

impl Display for  Dna {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl Display for  Rna {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl Sequence for Dna {
    fn get(&self) -> &str {
        &self.sequence.get()
    }
}

impl Dna {
    /// # Create a new Dna sequence.
    pub fn new(sequence: impl Into<String>) -> Result<Dna, SequenceError> {
        let sequence =
            strings::Sequences::new(DnaSequenceFactory(sequence.into()));
        match sequence {
            Ok(s) => Ok(Dna { sequence: s }),
            Err(e) => Err(e),
        }
    }

    /// # Transcribe the sequence into RNA
    pub fn transcribe(self) -> Rna {
        let rna = self.sequence.get().replace("t", "u").replace("T", "U");
        Rna::new(rna).unwrap()
    }
}

pub struct Rna {
    sequence: strings::Sequence,
}

impl Sequence for crate::sequence::Rna {
    fn get(&self) -> &str {
        &self.sequence.get()
    }
}
impl crate::sequence::Rna {
    /// # Create a new Dna sequence.
    pub fn new(
        sequence: impl Into<String>
    ) -> Result<crate::sequence::Rna, SequenceError> {
        let sequence =
            strings::Sequences::new(RnaSequenceFactory(sequence.into()));
        match sequence {
            Ok(s) => Ok(crate::sequence::Rna { sequence: s }),
            Err(e) => Err(e),
        }
    }
}

pub mod strings {
    use crate::sequence::strings::Alphabets::Dna;
    use std::cmp::Reverse;
    use std::collections::HashSet;
    use std::fmt::{Debug, Display, format};
    use std::str::Chars;
    use std::{error::Error, fmt};

    #[derive(Debug, PartialEq)]
    pub struct SequenceError {
        description: String,
        bad_char_set: HashSet<char>,
    }

    impl SequenceError {
        pub fn new(alphabet: Alphabet, badchars: HashSet<char>) -> SequenceError {
            SequenceError {
                description: format!("{alphabet:?}"),
                bad_char_set: badchars,
            }
        }

        fn description(badchars: HashSet<char>) -> String {
            format!("")
        }
    }

    impl fmt::Display for SequenceError {
        fn fmt(
            &self,
            f: &mut fmt::Formatter,
        ) -> fmt::Result {
            write!(
                f,
                "{:?}: Invalid characters in the sequence:\n {:?}",
                self.description,
                self.bad_char_set
            )
        }
    }

    #[derive(Debug)]
    pub struct Sequence {
        string: String,
        alphabet: Alphabet,
    }

    impl Sequence {
        pub fn new(
            string: impl Into<String>,
            alphabet: Alphabet,
        ) -> Result<Sequence, SequenceError> {
            let string = string.into();
            let unverified = Sequence {
                string: string,
                alphabet: alphabet,
            };
            let correctness = Self::check_alphabet(&unverified);
            match correctness {
                Some(x) => Err(x),
                _ => Ok(unverified),
            }
        }

        pub fn get(&self) -> &str {
            &self.string
        }

        fn check_alphabet(&self) -> Option<SequenceError> {
            // We accumulate all wrong characters.
            let mut wrong_set: HashSet<char> = HashSet::new();
            for char in self.string.chars() {
                let correct = self.alphabet.clone().contains(&char);
                if !correct {
                    wrong_set.insert(char);
                }
            }
            // Now lets check if the set is empty
            if !wrong_set.is_empty() {
                Some(SequenceError::new(self.alphabet.clone(), wrong_set))
            } else {
                None
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct Alphabet {
        set: HashSet<char>,
        case: bool,
        reverse: bool,
    }

    impl Alphabet {
        /// - `case` : Check the case of characters.
        pub fn new(
            chars: Vec<char>,
            case: bool,
            reverse: bool,
        ) -> Alphabet {
            let map: HashSet<char> = chars.into_iter().collect();
            if !case {
                todo!("case handling not yet present")
            }
            Alphabet {
                set: map,
                case: case,
                reverse: reverse,
            }
        }
        fn contains(
            self,
            lookupchar: &char,
        ) -> bool {
            match self.reverse {
                true => !self.set.contains(lookupchar),
                false => self.set.contains(lookupchar),
            }
        }
    }

    pub enum Alphabets {
        /// Set of ribonucleotides
        Rna,
        /// Set of Dna nucleotides
        Dna,
        /// The set of all characters.
        Any,
    }

    impl Alphabets {
        pub fn set(self) -> Alphabet {
            match self {
                Alphabets::Rna => Alphabet::new(
                    vec!['a', 'c', 'u', 'g', 'A', 'U', 'G', 'C'],
                    true,
                    false,
                ),
                Alphabets::Dna => Alphabet::new(
                    vec!['a', 'c', 't', 'g', 'A', 'T', 'G', 'C'],
                    true,
                    false,
                ),
                Alphabets::Any => Alphabet::new(vec![], true, true),
            }
        }
    }

    pub enum Sequences {
        /// Any free text
        Any(String),
        /// Dna
        Dna(String),
        Rna(String),
    }

    impl Sequences {
        pub fn new(self) -> Result<Sequence, SequenceError> {
            match self {
                Sequences::Dna(s) => Sequence::new(s, Alphabets::Dna.set()),
                Sequences::Any(s) => Sequence::new(s, Alphabets::Any.set()),
                Sequences::Rna(s) => Sequence::new(s, Alphabets::Rna.set()),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::sequence::strings::Alphabets::Dna;
    use crate::sequence::strings::{Alphabet, Alphabets, Sequence, SequenceError, Sequences};

    #[test]
    fn Any() {
        println!("{}", "any".to_string());
        let any =
            Sequences::new(Sequences::Any("This may be any text".to_string()));
        let sequence = any.unwrap();
    }
}
