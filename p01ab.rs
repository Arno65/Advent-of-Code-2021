///     Advent of Code 2021 - Day 1 task A & B
///     Solutions in Rust
///     Also solving in other languages like Haskell, Lisp, Swift and (old school) BASIC
///     (Ter leering ende vermaeck...)
///
///     The count of increased measurements is: 1692
///     The count of increased triplets is: 1724
///
///     (cl) by Arno Jacobs, 01-12-2021

use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

fn load_from_file(file_path: &str) -> Vec<i64> {
    let file = File::open(file_path).expect("{Rust} File not found.");
    let reader = BufReader::new(file);
    let numbers: Vec<i64> = reader
        .lines()
        .map(|line| line.unwrap().parse::<i64>().unwrap())
        .collect();
    return numbers;
}

fn count_increased(nl: Vec<i64> ) -> i64 {
    let mut rv = 0;
    for i in 0..&nl.len()-1 {
        if nl[i] < nl[i+1] { rv += 1; }
    }
    return rv;
}

fn sum_triplets(nl: Vec<i64>) -> Vec<i64> {
    let mut rv = Vec::new();
    for i in 0..&nl.len()-2 {
        rv.push(nl[i] + nl[i+1] + nl[i+2]);
    }
    return rv;
}


fn main() {
    let filename = "data/inputDay01_2021.txt";

    println!("\nAdvent of Code 2021 - day 1 - both parts in Rust");
// read file
    let nums = load_from_file( filename );
// work increasing numbers
    let c1 = count_increased( (& nums).to_vec());
// work increasing triplets
    let c2 = count_increased( sum_triplets( (& nums).to_vec())); 

    println!("The count of increased measurements is:  {}", c1);
    println!("The count of increased triplets is: {}", c2);
    println!("0K.\n");
}

