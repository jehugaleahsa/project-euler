struct RangeData {
    current: i32,
    step: i32,
    end: i32
}

impl RangeData {
    fn new(start: i32, end: i32, step: i32) -> RangeData {
        RangeData {
            current: start,
            end: end,
            step: step
        }
    }
}

impl Iterator for RangeData {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.step > 0 && self.current > self.end) || (self.step < 0 && self.current < self.end) {
            None
        } else {
            let old = self.current;
            self.current += self.step;            
            Some(old)
        }
    }
}

fn range_step(start: i32, end: i32, step: i32) -> RangeData {
    RangeData::new(start, end, step)
}

fn to_digits(value: u32) -> Vec<u32> {
    let mut parts = vec![];
    let mut quotient = value;
    if quotient == 0 {
        parts.push(quotient);
    } else {
        while quotient != 0 {
            let remainder = quotient % 10;
            parts.push(remainder);
            quotient /= 10;
        }
        parts.reverse();
    }
    parts
}

fn is_palindrome(value: u32) -> bool {
    let digits = to_digits(value);
    let mut front_index = 0;
    let mut back_index = digits.len() - 1;
    while front_index < back_index {
        let front = digits[front_index];
        let back = digits[back_index];
        if front != back {
            return false;
        }
        front_index += 1;
        back_index -= 1;
    }
    return true
}

fn find_max_palindrome() -> i32 {
    let mut max = 0;
    for x in range_step(999, 100, -1) {
        if x * x < max {
            break;
        }
        for y in range_step(x, 100, -1) {
            let product = x * y;
            if product < max {
                break;
            } else if product > max && is_palindrome(product as u32) {
                max = product;
            }
        }
    }
    max
}

fn main() {
    let palindrome = find_max_palindrome();
    println!("{}", palindrome);
}
