//! # The Sequence module
//! You initiate a new object using New. Then you handle the sequence error. After this you use the methods.
//! ```rust
//! let input = "AAAACCCGGT".to_string();
//! let expected = "ACCGGGTTTT".to_string();
//! let result = Dna::new(input).
//!     unwrap().
//!     reverse().
//!     complement().
//!     get().
//!     to_string();
//! ```

use crate::lib::sequence::strings::Sequences::Any as TextSequenceFactory;
use crate::lib::sequence::strings::Sequences::Dna as DnaSequenceFactory;
use crate::lib::sequence::strings::Sequences::Rna as RnaSequenceFactory;
use crate::lib::sequence::strings::{Alphabet, Alphabets, SequenceError};
use duplicate::duplicate_item;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};

// https://stackoverflow.com/a/61467564/15753558 to duplicate repeat implementations.

/// # Facilities to deal with sequences on a fundamental level.
/// All sequences of text have an alphabet, a getter and a method with with you can make a new sequence of the same
/// type.
pub trait Sequence: fmt::Display {
    /// Get the string of the sequence.
    fn get(&self) -> &str;

    /// Get the [Alphabet] of the sequence.
    fn get_alphabet(&self) -> Alphabet;

    /// Making a new sequence of the same type using a
    fn new(string: impl Into<String>) -> Result<Self, SequenceError>
    where
        Self: Sized;
}

pub trait Length {
    fn length(&self) -> i32;
}

pub trait Reverse {
    fn reverse(&self) -> Box<Self>;
}

impl<T: Sequence> Reverse for T {
    /// # Reverse the sequence by creating a new sequence that is reversed.
    fn reverse(&self) -> Box<T> {
        let value = self.get();
        let reversed: String = value.chars().rev().collect();
        Box::new(T::new(reversed).unwrap())
    }
}

pub trait Frequency {
    fn frequency(&self) -> HashMap<char, i32>;
}

/// TODO Make the frequency API use the known alphabet.
impl<T: ?Sized + Sequence> Frequency for T {
    /// # Determine the frequency of a sequence.
    ///
    /// # Returns
    /// The frequency of the sequence. If a letter that is in the alphabet does not exist in the sequence, a 0 is given.
    fn frequency(&self) -> HashMap<char, i32> {
        let seq = self.get();
        let mut letter_counts: HashMap<char, i32> = HashMap::new();
        let set = self.get_alphabet().set();
        for c in set {
            let _ = *letter_counts.entry(c).or_insert(0);
        }
        for c in seq.chars() {
            *letter_counts.entry(c).or_insert(0) += 1;
        }
        letter_counts
    }
}

impl<T: ?Sized + Sequence> Length for T {
    /// # Determine the length of a sequence.
    ///
    /// # Returns
    /// The length of the sequence.
    fn length(&self) -> i32 {
        let seq = self.get();
        seq.len() as i32
    }
}

/// A Sequence build using [strings::Alphabets::Dna].
#[derive(Clone, Debug)]
pub struct Dna {
    sequence: strings::Sequence,
    alphabet: Alphabet,
}

impl Display for Dna {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl Display for Text {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}
impl Display for Rna {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl Sequence for Dna {
    fn get(&self) -> &str {
        &self.sequence.get()
    }

    fn get_alphabet(&self) -> Alphabet {
        self.alphabet.clone()
    }

    fn new(sequence: impl Into<String>) -> Result<Self, SequenceError> {
        Dna::new(sequence)
    }
}

impl Dna {
    /// # Create a new Dna sequence.
    pub fn new(sequence: impl Into<String>) -> Result<Dna, SequenceError> {
        let sequence =
            strings::Sequences::new(DnaSequenceFactory(sequence.into()));
        match sequence {
            Ok(s) => Ok(Dna {
                sequence: s,
                alphabet: Alphabets::Dna.set(),
            }),
            Err(e) => Err(e),
        }
    }

    /// # Transcribe the sequence into RNA
    pub fn transcribe(self) -> Rna {
        let rna = self.sequence.get().replace("t", "u").replace("T", "U");
        Rna::new(rna).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Rna {
    sequence: strings::Sequence,
    alphabet: Alphabet,
}

/// A Sequence build using [strings::Alphabets::Rna].
impl Sequence for Rna {
    fn get(&self) -> &str {
        &self.sequence.get()
    }
    fn get_alphabet(&self) -> Alphabet {
        self.alphabet.clone()
    }
    fn new(sequence: impl Into<String>) -> Result<Self, SequenceError> {
        Rna::new(sequence)
    }
}
impl Rna {
    /// # Create a new Dna sequence.
    pub fn new(sequence: impl Into<String>) -> Result<Rna, SequenceError> {
        let sequence =
            strings::Sequences::new(RnaSequenceFactory(sequence.into()));
        match sequence {
            Ok(s) => Ok(Rna {
                sequence: s,
                alphabet: Alphabets::Rna.set(),
            }),
            Err(e) => Err(e),
        }
    }
}

pub trait Complement {
    fn complement(self) -> Self;
}

impl Complement for Dna {
    /// Complement of an Dna sequence.
    fn complement(self) -> Self {
        let seq: &str = self.get();
        let mut replace: Vec<char> = Vec::new();
        for seq_char in seq.chars().into_iter() {
            let complement = match seq_char {
                'a' => 't',
                'A' => 'T',
                't' => 'a',
                'T' => 'A',
                'C' => 'G',
                'c' => 'g',
                'G' => 'C',
                'g' => 'c',
                _ => {
                    panic!()
                }
            };
            replace.push(complement);
        }
        let outstring: String = replace.iter().collect();
        Self::new(outstring).unwrap()
    }
}

pub trait GcFraction {
    fn gc(&self) -> f32;
}

#[duplicate_item(name; [Dna]; [Rna])]
impl GcFraction for name {
    fn gc(&self) -> f32 {
        let freq = &self.frequency();
        let count_cg = freq.get(&'C').unwrap()
            + freq.get(&'c').unwrap()
            + freq.get(&'G').unwrap()
            + freq.get(&'g').unwrap();
        // When we divide we must first make sure to cast to f32.
        let fraction = count_cg as f32 / self.length() as f32;
        fraction
    }
}

/// A Sequence build using [strings::Alphabets::Any].
#[derive(Clone)]
pub struct Text {
    sequence: strings::Sequence,
    alphabet: Alphabet,
}

impl Sequence for Text {
    fn get(&self) -> &str {
        &self.sequence.get()
    }
    fn get_alphabet(&self) -> Alphabet {
        self.alphabet.clone()
    }
    fn new(sequence: impl Into<String>) -> Result<Self, SequenceError> {
        Text::new(sequence)
    }
}
impl Text {
    /// # Create a new Dna sequence.
    pub fn new(sequence: impl Into<String>) -> Result<Text, SequenceError> {
        let sequence =
            strings::Sequences::new(TextSequenceFactory(sequence.into()));
        match sequence {
            Ok(s) => Ok(Text {
                sequence: s,
                alphabet: Alphabets::Any.set(),
            }),
            Err(e) => Err(e),
        }
    }
}

// TODO do transcription by Into Rna from DNA
impl Into<Rna> for Dna {
    /// # Transcribe the sequence into [Rna].
    fn into(self) -> Rna {
        self.transcribe()
    }
}

/// # The internals of Sequence
///
/// There is a Sequences struct that makes a new defined sequence.
pub mod strings {

    use std::collections::HashSet;
    use std::fmt::Debug;

    use std::{error::Error, fmt};

    /// # Representation of a miss-mapping to a alphabet.
    /// When a [Sequence::new()] is created, a check is made to determine if all letters from the input string are
    /// included in the expected [Alphabet]. If this not the case, this mistake should be reported to the upper function
    /// using this struct.
    #[derive(Debug, PartialEq, Clone)]
    pub struct SequenceError {
        /// What the error is about, describes the name of the alphabet.
        description: String,
        /// The set of characters that were not accepted from the sequence that raised the error.
        bad_char_set: HashSet<char>,
    }

    impl SequenceError {
        pub fn new(
            alphabet: Alphabet,
            badchars: HashSet<char>,
        ) -> SequenceError {
            SequenceError {
                description: alphabet.name,
                bad_char_set: badchars,
            }
        }

        fn description(desc: String) -> String {
            desc
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
                self.description, self.bad_char_set
            )
        }
    }

    // todo check if traits carry overhead.

    /// # A general sequence
    /// Contains facilities for checking. Any functions that use the data should be implemented as traits.
    #[derive(Debug, Clone)]
    pub struct Sequence {
        /// Data of the sequence.
        string: String,
        /// Expected alphabet of the sequence.
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

        /// # Validate the sequence
        /// To determine of it contains letters that are not part of the alphabet.
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

    /// # An alphabet
    #[derive(Clone, Debug)]
    pub struct Alphabet {
        /// Is the set of characters what will be checked depending on `reverse`.
        set: HashSet<char>,
        /// Unimplemented.
        case: bool,
        /// Whether the set is supposed to be reversed: if `set` is supposed to be allowed, or disallowed.
        reverse: bool,
        /// Name of the alphabet.
        name: String,
    }

    impl Alphabet {
        /// - `case` : Check the case of characters.
        pub fn new(
            chars: Vec<char>,
            case: bool,
            reverse: bool,
            name: String,
        ) -> Alphabet {
            let map: HashSet<char> = chars.into_iter().collect();
            if !case {
                todo!("case handling not yet present")
            }
            Alphabet {
                set: map,
                case: case,
                reverse: reverse,
                name: name,
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

        pub fn name(self) -> String {
            self.name
        }

        /// Getter for the set variable
        pub fn set(self) -> HashSet<char> {
            self.set
        }
    }

    /// # The defined alphabets
    ///
    /// An alphabet can be obtained using the set method.
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
            let name = match self {
                Alphabets::Dna => "Dna".to_string(),
                Alphabets::Rna => "Rna".to_string(),
                Alphabets::Any => "Any text".to_string(),
            };
            match self {
                Alphabets::Rna => Alphabet::new(
                    vec!['a', 'c', 'u', 'g', 'A', 'U', 'G', 'C'],
                    true,
                    false,
                    name,
                ),
                Alphabets::Dna => Alphabet::new(
                    vec!['a', 'c', 't', 'g', 'A', 'T', 'G', 'C'],
                    true,
                    false,
                    name,
                ),
                Alphabets::Any => Alphabet::new(vec![], true, true, name),
            }
        }
    }

    /// # Method to produce a [Sequence] with a defined [Alphabet].
    pub enum Sequences {
        /// Any free text
        Any(String),
        /// Dna
        Dna(String),
        /// Rna
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
    use crate::lib::sequence::strings::Sequences::Dna;
    use crate::lib::sequence::strings::{Sequence, Sequences};
    use crate::lib::sequence::{GcFraction, Length, Reverse, Text};

    #[test]
    fn any() {
        println!("{}", "any".to_string());
        let any =
            Sequences::new(Sequences::Any("This may be any text".to_string()));
        let _sequence = any.unwrap();
    }

    #[test]
    fn sequence_traits() {
        let any = Text::new("0123456789").unwrap();
        let len = any.length();
        assert_eq!(10, len);

        let reversed = any.reverse();
        print!("{reversed}");
    }

    #[test]
    fn cg_percentage() {
        let seq = crate::lib::sequence::Dna::new("AAAA").unwrap();
        let seqCg = seq.gc();
        assert_eq!(seqCg, 0.0);
        let seq = crate::lib::sequence::Dna::new("AAGG").unwrap();
        let seqCg = seq.gc();
        assert_eq!(seqCg, 0.5);
        let seq = crate::lib::sequence::Dna::new("CCGG").unwrap();
        let seqCg = seq.gc();
        assert_eq!(seqCg, 1.0);
    }
}
