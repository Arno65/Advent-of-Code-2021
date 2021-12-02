///     Advent of Code 2021 - Day 2 task A & B
///     Solutions in Swift
///     Also solving in other languages like Haskell, Lisp and Rust 
///     (Ter leering ende vermaeck...)
///
///     With the first set of rules the result if you multiply the final 
///     horizontal position by the final depth is: 1762050
///     With the second set of rules the result if you multiply the final 
///     horizontal position by the final depth is: 1855892637
///
///     (cl) by Arno Jacobs, 02-12-2021


import Cocoa

let filename = "Code/Advent/AoC_2021/data/inputDay02_2021.txt"

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


func tripOne(_ dl: String, _ sl: [Int]) -> Int {
    var hp = 0
    var vp = 0 
    for i in 0...dl.count-1 {
        switch dl[i] {
            case "u":   vp -= sl[i]
            case "d":   vp += sl[i]
            default:    hp += sl[i]
        }
    }
    return (hp*vp)
}
 
func tripTwo(_ dl: String, _ sl: [Int]) -> Int {
    var hp = 0
    var vp = 0 
    var aim = 0
    for i in 0...dl.count-1 {
        switch dl[i] {
            case "u":   aim -= sl[i]
            case "d":   aim += sl[i]
            default:    hp += sl[i]
                        vp += aim * sl[i]
        }
    }
    return (hp*vp)
}
 
func parse(_ dlines: [String.SubSequence]) -> (String,[Int]) {
    var dlist: String = ""
    var slist = [Int]()
    for direction in dlines {
        let sdirection = String(direction)
        let dirSteps = sdirection.components(separatedBy: " ")
        dlist.append(dirSteps[0][0])        // array of first characters ('fdu')
        slist.append(Int(dirSteps[1])!)     // array of number of steps
    }
    return ( dlist, slist)
}

// Main program
//
print ("Advent of Code 2021 - day 2 - both parts in Swift")

if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
{
    let fileURL = dir.appendingPathComponent(filename)

    do {
        // Read data
        let directions = try String(contentsOf: fileURL, encoding: .utf8)

        var directionsList: String = ""
        var stepsList = [Int]()
        ( directionsList, stepsList ) = parse(directions.lines)

        // Task One
        var nCnt = tripOne( directionsList, stepsList)
        print ("The product of the end position after task one is: \(nCnt)")

        // Task Two 
        nCnt = tripTwo( directionsList, stepsList)
        print ( "The product of the end position after task two is: \(nCnt)")
    }
    catch { print ("Read error?") }

    print ("0K.\n")
}
