use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::str::Chars;
use std::{error::Error, fmt};
use std::cmp::Reverse;
use crate::sequence::Alphabets::Dna;

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
    reverse: bool,
}

impl Alphabet {
    /// - `case` : Check the case of characters.
    pub fn new(
        chars: Vec<char>,
        case: bool,
        reverse: bool
    ) -> Alphabet {
        let map: HashSet<char> = chars.into_iter().collect();
        if !case {
            todo!("case handling not yet present")
        }
        Alphabet {
            set: map,
            case: case,
            reverse: reverse
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
    Dna,
    /// The set of all characters.
    Any,
}

impl Alphabets {
    pub fn set(self) -> Alphabet {
        match self {
            Alphabets::Dna => Alphabet::new(vec!['a', 'c', 't', 'g', 'A', 'T', 'G', 'C'],
                                            true, false),
            Alphabets::Any => Alphabet::new(vec![], true, true),
        }
    }
}

pub enum Sequences {
    /// Any free text
    Any(String),
    Dna(String),
}

impl Sequences {
    pub fn new(self) -> Result<Sequence, SequenceError>{
        match self {
            Sequences::Dna(s) => Sequence::new(s, Alphabets::Dna.set()),
            Sequences::Any(s) => Sequence::new(s, Alphabets::Any.set()),
        }
    }
}
#[cfg(test)]
mod test {
    use crate::sequence::{Alphabet, Sequence, SequenceError, Sequences};
    use crate::sequence::Alphabets::Dna;

    #[test]
    fn DnaError() {
        let expected_wrong = vec!['n', 'i', 'y', 'r', 's', ' ', 'h', 'e', 'o'];
        let wrong_set = expected_wrong.into_iter().collect();
        let expected_error = SequenceError::new(wrong_set);
        let dna = Sequences::new(Sequences::Dna("This contains any characters".to_string()));
        match dna {
            Err(e) => assert_eq!(expected_error, e),
            _ => panic!("The error does not work!")
        }
    }

    #[test]
    fn Any() {
        println!("{}", "any".to_string());
        let any = Sequences::new(Sequences::Any("This may be any text".to_string()));
        let sequence = any.unwrap();
    }
}
