///     Advent of Code 2021 - Day 1 task A & B
///     Solutions in Swift
///     Also solving in other languages like Haskell, Lisp, Rust and (old school) BASIC
///     (Ter leering ende vermaeck...)
///
///     The count of increased measurements is: 1692
///     The count of increased triplets is: 1724
///
///     (cl) by Arno Jacobs, 01-12-2021


import Cocoa

let filename = "Code/Advent/AoC_2021/data/inputDay01_2021.txt"

// Helper code for reading textfiles and converting String into Int
extension StringProtocol {
    var lines: [SubSequence] { split(whereSeparator: \.isNewline) }
}

extension String {
    subscript (index: Int) -> Character {
        let charIndex = self.index(self.startIndex, offsetBy: index)
        return self[charIndex]
    }

    subscript (range: Range<Int>) -> Substring {
        let startIndex = self.index(self.startIndex, offsetBy: range.startIndex)
        let stopIndex = self.index(self.startIndex, offsetBy: range.startIndex + range.count)
        return self[startIndex..<stopIndex]
    }
}


func countIncreases(for numbers: [Int]) -> Int {
    var cnt = 0 
    for i in 0...numbers.count-2 {
        if numbers[i] < numbers[i+1] { cnt += 1 }
    }
    return cnt
}
 

func sumTriplets(for numbers: [Int]) -> [Int] {
    var ra = [Int]()
    for i in 0...numbers.count-3 {
        ra.append(numbers[i] + numbers[i+1] + numbers[i+2])
    }
    return ra
}


// Main program
//
print ("Advent of Code 2021 - day 1 - both parts in Swift")

if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
{
    let fileURL = dir.appendingPathComponent(filename)

    do {
        // Read data
        let readMap = try String(contentsOf: fileURL, encoding: .utf8)
        let numbers = readMap.lines.compactMap {Int($0)!}
       
        // Task One
        var nCnt = countIncreases(for: numbers)
        print ("The count of increased measurements is: \(nCnt)")
        
        // Task Two 
        nCnt = countIncreases(for: sumTriplets (for: numbers))
        print ("The count of increased triplets is:     \(nCnt)")
    }
    catch { print ("Read error?") }

    print ("0K.\n")
}
