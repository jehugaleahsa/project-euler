struct Seive {
    current: u64,
    primes: Vec<u64>
}

impl Seive {
    fn new() -> Seive {
        Seive {
            current: 1u64,
            primes: vec![]
        }
    }

    fn is_prime(&self, value: u64) -> bool {
        let half = value / 2;
        for prime in &self.primes {
            if value % *prime == 0 {
                return false;
            } else if *prime > half {
                return true;
            }
        }
        return true;
    }
}

impl Iterator for Seive {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next = self.current + 1;
        while !self.is_prime(next) {
            next += 1;
        }
        self.current = next;
        self.primes.push(next);
        Some(next)
    }
}

fn getLargestFactor(value: u64) -> u64 {
    let mut remaining = value;
    let seive = Seive::new();
    let half = value / 2;
    for prime in seive.take_while(|p| *p < half ) {
        if remaining % prime == 0 {
            remaining /= prime;
            if remaining == 1 {
                return prime;
            }
        }
    }
    remaining
}

fn main() {
    let factor = getLargestFactor(600_851_475_143u64);
    println!("{}", factor);
}
