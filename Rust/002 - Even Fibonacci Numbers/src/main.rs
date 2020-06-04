fn main() {
    imperative();
    functional();
}

fn imperative() {
    let mut first = 0u64;
    let mut second = 1u64;
    let mut sum = 0u64;
    while second < 4_000_000 {
        let next = first + second;
        if next % 2 == 0 {
            sum += next;
        }
        first = second;
        second = next;
    }
    println!("{}", sum);
}

struct Fib {
    first: u64,
    second: u64
}

impl Fib {
    fn new() -> Fib {
        Fib { 
            first: 0, 
            second: 1 
        }
    }
}

impl Iterator for Fib {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.first + self.second;
        self.first = self.second;
        self.second = next;
        Some(next)
    }
}

fn functional() {
    let fib = Fib::new();
    let sum: u64 = fib
        .take_while(|x| *x <= 4_000_000u64)
        .filter(|x| x % 2 == 0)
        .sum();
    println!("{}", sum);
}