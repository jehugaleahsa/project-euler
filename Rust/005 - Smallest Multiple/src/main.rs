fn gcd(a: i64, b: i64) -> i64 {
    let mut left = a;
    let mut right = b;
    while right != 0 {
        let temp = right;
        right = left % right;
        left = temp;
    }
    left
}

fn lcm(a: i64, b: i64) -> i64 {
    (a * b) / gcd(a, b)
}

fn main() {
    let result = (1i64..=20i64).fold(1, lcm);
    println!("{}", result);
}
