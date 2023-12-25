/// Calculates the fibonacci sequence.
/// Operates using recursion, large values of `n` lead to stack overflow.
pub fn fibonacci(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

/// Calculates the fibonacci rabbit sequence.
/// `n` is the amount of generations, `breeding` is the amount of offspring each generation produces.
/// This method operates using recursion. Large values of `n` lead to stack overflow.
pub fn fibonacci_rabbits(
    n: i64,
    breeding: i64,
) -> i64 {
    if n <= 1 {
        n
    } else {
        fibonacci_rabbits(n - 1, breeding)
            + fibonacci_rabbits(n - 2, breeding) * breeding
    }
}

#[cfg(test)]
mod test {
    use crate::lib::population::{fibonacci, fibonacci_rabbits};

    #[test]
    pub fn test_fib() {
        let value = fibonacci(6);
        assert_eq!(value, 8);
        let value = fibonacci(0);
        assert_eq!(0, value);
        assert_eq!(13, fibonacci(7));
        assert_eq!(21, fibonacci(8));
    }

    #[test]
    pub fn test_rabbits() {
        let value = fibonacci_rabbits(5, 3);
        assert_eq!(value, 19);
    }

    #[test]
    pub fn test_rabbits_big() {
        let value = fibonacci_rabbits(40, 5);
    }
}
