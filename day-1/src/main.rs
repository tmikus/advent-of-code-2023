use std::io::{self, BufRead};

const SIMPLE_PATTERNS: &[(&str, i32)] = &[
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
];
const FULL_PATTERS: &[(&str, i32)] = &[
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
];

struct PatternMatcher {
    matcher_position: usize,
    pattern_chars: Vec<char>,
    result: i32,
}

impl PatternMatcher {
    fn new(pattern: &str, result: i32) -> PatternMatcher {
        PatternMatcher {
            matcher_position: 0,
            pattern_chars: pattern.chars().collect(),
            result,
        }
    }

    fn try_match(&mut self, input: char) -> Option<i32> {
        if self.pattern_chars[self.matcher_position] != input {
            self.matcher_position = 0;
            if self.pattern_chars[0] != input {
                return None
            }
        }
        self.matcher_position += 1;
        if self.matcher_position == self.pattern_chars.len() {
            self.matcher_position = 0;
            return Some(self.result);
        }
        return None;
    }
}

fn find_number(matchers: &mut [PatternMatcher], line: &str) -> i32 {
    let (left, right) = find_outside_numbers(matchers, line);
    left * 10 + right
}

fn find_outside_numbers(matchers: &mut [PatternMatcher], line: &str) -> (i32, i32) {
    let mut left_most: Option<i32> = None;
    let mut right_most: Option<i32> = None;
    for char in line.chars() {
        for matcher in matchers.iter_mut() {
            let value = match matcher.try_match(char) {
                Some(v) => v,
                None => continue,
            };
            if left_most.is_none() {
                left_most = Some(value);
            }
            right_most = Some(value);
        }
    }
    (left_most.unwrap(), right_most.unwrap())
}

fn init_matchers(patterns: &[(&'static str, i32)]) -> Vec<PatternMatcher> {
    patterns.into_iter()
        .map(|(pattern, result)| PatternMatcher::new(pattern, *result))
        .collect()
}

fn main() {
    let mut matchers = init_matchers(FULL_PATTERS);
    let stdin = io::stdin();
    let mut result = 0;
    for line in stdin.lock().lines() {
        let line = match line {
            Ok(v) => v,
            _ => continue,
        };
        if line.is_empty() {
            continue
        }
        result += find_number(&mut matchers, &line);
    }
    println!("{}", result)
}
