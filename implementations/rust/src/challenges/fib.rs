//! # Solution to recursive rabbits.

use crate::lib::population::fibonacci_rabbits as fb;

/// # Calculate the fibonacci rabbits.
/// String is `<generations>` and `<breeding rate>` both inputs must be numbers if this is
/// not the case the function will panic.
pub fn fibonacci_rabbits(input: impl Into<String>) -> i64 {
    let input: String = input.into();
    let input: Vec<&str> = input.split(" ").collect();
    if input.len().clone() == 2 {
        let generations: i32 = input.get(0).unwrap().parse().unwrap();
        let breeding: i32 = input.get(1).unwrap().parse().unwrap();
        let output = fb(generations.into(), breeding.into());
        output
    } else {
        panic!("You should have only two input numbers!");
    }
}

#[cfg(test)]
mod test {
    use crate::challenges::fib::fibonacci_rabbits;

    #[test]
    fn example_data() {
        let input = "5 3".to_string();
        let result = fibonacci_rabbits(input);
        assert_eq!(19, result);
    }

    #[test]
    fn wrong_answer() {
        // first one
        let input = "31 4".to_string();
        let result = fibonacci_rabbits(input);
        assert_ne!(1145241941, result, "Incorrect answer from known results.");

        // second one
        let input = "31 2".to_string();
        let result = fibonacci_rabbits(input);
        assert_ne!(
            1117836738901, result,
            "Incorrect answer from known results."
        );
    }

    #[test]
    #[should_panic]
    fn example_data_wrong() {
        let input = "5 a".to_string();
        let result = fibonacci_rabbits(input);
        assert_eq!(19, result);
    }

    #[test]
    fn correct_answer() {
        let input = "29 4".to_string();
        let result = fibonacci_rabbits(input);
        assert_eq!(170361678269, result);
    }
}
