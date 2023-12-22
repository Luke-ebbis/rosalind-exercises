use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::str::Chars;
use std::{error::Error, fmt};

#[derive(Debug, PartialEq)]
pub struct SequenceError {
    description: String,
    bad_char_set: HashSet<char>,
}

impl SequenceError {
    pub fn new(badchars: HashSet<char>) -> SequenceError {
        let description = Self::description(badchars.clone());
        SequenceError {
            description,
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
            "Invalid characters in the sequence:\n {:?}",
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
            _ => Ok(unverified)
        }
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
            Some(SequenceError::new(wrong_set))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct Alphabet {
    set: HashSet<char>,
    case: bool,
}

impl Alphabet {
    pub fn new(
        chars: Vec<char>,
        case: bool,
    ) -> Alphabet {
        let map: HashSet<char> = chars.into_iter().collect();
        Alphabet {
            set: map,
            case: case,
        }
    }
    pub fn contains(
        self,
        lookupchar: &char,
    ) -> bool {
        self.set.contains(lookupchar)
    }
}

pub enum Alphabets {
    Dna,
}

impl Alphabets {
    pub fn set(self) -> Alphabet {
        match self {
            Alphabets::Dna => Alphabet::new(vec!['a', 'c', 't', 'g'], true),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::sequence::{Alphabet, Sequence, SequenceError};
    use crate::sequence::Alphabets::Dna;

    #[test]
    fn sequence_error() {
        let expected_wrong = vec!['n', 'i', 'y', 'r', 's', ' ', 'T', 'h', 'e', 'o'];
        let wrong_set = expected_wrong.into_iter().collect();
        let expected_error = SequenceError::new(wrong_set);
        let sequence = Sequence::new(
            "This contains any characters",
            Dna.set(),
        );
        match sequence {
            Err(e) => assert_eq!(expected_error, e),
            _ => panic!("The error does not work!")
        }
    }
}
