fn sum_squares(n: i64) -> i64 {
    (1..=n)
        .map(|x| x * x)
        .sum()
}

fn square_sum(n: i64) -> i64 {
    let sum: i64 = (1..=n).sum();
    sum * sum
}

fn sum_squares_fast(n: i64) -> i64 {
    (2 * n + 1) * (n + 1) * n / 6
}

fn square_sum_fast(n: i64) -> i64 {
    let sum = n * (n + 1) / 2;
    sum * sum
}

fn main() {
    let squares_summed = sum_squares(100);
    let sum_squared = square_sum(100);
    println!("Squares Summed: {}", squares_summed);
    println!("Sum Squared: {}", sum_squared);
    let difference = sum_squared - squares_summed;
    println!("Difference: {}", difference);

    let squares_summed = sum_squares_fast(100);
    let sum_squared = square_sum_fast(100);
    println!("Squares Summed: {}", squares_summed);
    println!("Sum Squared: {}", sum_squared);
    let difference = sum_squared - squares_summed;
    println!("Difference: {}", difference);
}
