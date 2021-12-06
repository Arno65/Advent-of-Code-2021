///     Advent of Code 2021 - Day 6 task A & B
///     Solutions in Rust
///     Also solving in other languages like Haskell and Lisp 
///     (Ter leering ende vermaeck...)
///
///     The number of lanternfish after  80 days is: 388739
///     The number of lanternfish after 256 days is: 1741362314973
///
///     (cl) by Arno Jacobs, 06-12-2021

use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

/// Read one file string line like "1,2,3,4" to Vec<u64> [1,2,3,4]
fn lines_from_file(filename: &str) -> Vec<u64> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines().next().unwrap().unwrap()
        .split(',').map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().unwrap())
        .collect()
}


/// Count for digit 'd' the number of digits in the list
fn count_digits( d: &u64, fl: &Vec<u64>) -> u64 {
    let mut cnt: u64  = 0;
    for fd in fl {
        if fd == d { cnt += 1; }
    }
    return cnt;
}

/// Count all digits [0..9] in the list
fn parse(fl: Vec<u64>) -> Vec<u64> {
    let mut digits: Vec<u64> = [].to_vec();
    for ix in 0..9 {
        digits.push ( count_digits ( &ix, &fl));
    }
    return digits;
}

/// Work the rules - work the timers - count per digit
/// The minimum timer setting is 0
/// The maximum timer setting is 8
/// The number iterations will be the number of days + 1
/// The sum of the count per digit is the return value
fn endless_lantern_fishes(cnt: u64, plst: Vec<u64> ) -> u64 {
    let mut iter_lst: Vec<u64> = plst.to_vec();
    let mut ihl: Vec<u64> = plst.to_vec();
    let mut sc: u64 = 0;
    let mut ip6: u64;
    let mut ip7: u64;
    let mut ip8: u64;

    for _c in 0..cnt+1 { 
        ip6 = iter_lst[0] + iter_lst [7];
        ip7 = iter_lst[8];
        ip8 = iter_lst[0];
        for i in 0..6 { ihl[i] = iter_lst[i+1]; }
        ihl[6] = ip6;
        ihl[7] = ip7;
        ihl[8] = ip8;
        iter_lst = ihl.to_vec();
    }

    // Sum the list - and return that sum value
    for i in 0..8 { sc += iter_lst[i]; }
    return sc;
}   


fn main() {
    let filename = "data/inputDay06_2021.txt";
    let i1 = 80;    // Number of days for part 1
    let i2 = 256;   // Number of days for part 2

    println!("Advent of Code 2021 - day 6 - both parts in Rust");
    let day6 = lines_from_file( filename );         // read the dataset
    let plst = parse(day6);                         // First parse the dataset
    let p1   = endless_lantern_fishes( i1, plst.to_vec());  // Work part 1
    let p2   = endless_lantern_fishes( i2, plst.to_vec());  // Work part 2
 
    println!("The number of lanternfish after  {} days is: {}", i1, p1);
    println!("The number of lanternfish after {} days is: {}", i2, p2);
    println!("0K.\n");
}



