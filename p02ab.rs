///     Advent of Code 2021 - Day 2 task A & B
///     Solutions in Rust
///     Also solving in other languages like Haskell, Lisp and Swift
///     (Ter leering ende vermaeck...)
///
///     With the first set of rules the result if you multiply the final 
///     horizontal position by the final depth is: 1762050
///     With the second set of rules the result if you multiply the final 
///     horizontal position by the final depth is: 1855892637
///
///     (cl) by Arno Jacobs, 02-12-2021

use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;

fn lines_from_file(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

// Get the number of steps
fn get_integer(dir: &str) -> u32 {
    let ns = dir.chars().skip_while(|c| !c.is_digit(10)).collect::<String>();
    return ns.parse::<u32>().unwrap();
}

fn both_tasks(directions: Vec<String>) -> (u32,u32) {
    let mut hp  = 0;
    let mut vp1 = 0;
    let mut vp2 = 0;
    let mut aim = 0;
    for direction in directions {
        let mv   = &direction[..1].to_string();     // First character of the direction
        let stps = get_integer(&direction);         // Get the number of steps
        if mv == "f" { hp  += stps; vp2 += aim * stps;  }
        if mv == "d" { vp1 += stps; aim += stps;        }
        if mv == "u" { vp1 -= stps; aim -= stps;        }
    }
    return ( hp*vp1, hp*vp2);
}


fn main() {
    let filename = "data/inputDay02_2021.txt";

    println!("Advent of Code 2021 - day 2 - both parts in Rust");
// read file
    let directions = lines_from_file( filename );
// work data - one function for both tasks
    let (p1,p2) = both_tasks(directions);

    println!("The product of the end position after task one is: {}", p1);
    println!("The product of the end position after task two is: {}", p2);
    println!("0K.\n");
}

