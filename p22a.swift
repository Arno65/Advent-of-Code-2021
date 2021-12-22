///     Advent of Code 2021 - Day 15 task A only - brute force won't work on part B
///     This brute force solution will run in 60 ms on my MacBook.
///
///     Solutions in Swift
///     (Ter leering ende vermaeck...)
///
///     After initialize, the number of cubes ON is: 655005
///     
///     (cl) by Arno Jacobs, 22-12-2021

import Cocoa

let filename = "Code/Advent/AoC_2021/data/inputDay22_2021.txt"

let numCoordinates  = 6
var sRange: Int     = 102           // enough space to fill...
let offset: Int     = sRange / 2
let cubeOFF         = 0
let cubeON          = 1
var fill            = cubeOFF
let separators      = CharacterSet(charactersIn: "=.,")

// Create minimum space and 
var space: [[[Int]]] = Array(repeating: 
                        Array(repeating: 
                            Array(repeating: cubeOFF, count: sRange),
                            count: sRange),
                        count: sRange)


// The read & parse function
// Read lines like:
//  on x=11..13,y=11..13,z=11..13
//  off x=9..11,y=9..11,z=9..11
// Per line, fill the 3D-space with Cubes ON or Cubes OFF
//
func readAndParse () {
    if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
    {
        let fileURL = dir.appendingPathComponent(filename)
        do {
            // Read data
            let readGrid = try String(contentsOf: fileURL, encoding: .utf8)            
            let lines = readGrid.split(separator: "\n")

            for l in lines { 
                var cc = 0
                var c3d = Array(repeating: 0, count: 6)
                let state = String( l.prefix(2))
                let ipl = l.components(separatedBy: separators)
                for s in ipl {
                    if let n = Int(s) { // Only work the 6 numbers per line...
                        c3d[cc] = n
                        cc += 1
                        if cc==numCoordinates { cc = 0 } // Reset coordinate counter 'cc'
                    }
                }
                if state == "on" { fill = cubeON } else { fill = cubeOFF }
                fillSpace (xyzFT: c3d, filler: fill)
            }
        }
        catch { print ("Read error?") }
    }
}

func fillSpace (xyzFT: [Int], filler: Int ) {
    let xf = xyzFT[0] + offset 
    let xt = xyzFT[1] + offset 
    let yf = xyzFT[2] + offset 
    let yt = xyzFT[3] + offset 
    let zf = xyzFT[4] + offset 
    let zt = xyzFT[5] + offset 
    var inRange = true
    for c in xyzFT { if c < -50 || c > 50  { inRange = false } }
    if inRange {            // Only change the space if the coordinates are all in range
        for x in xf...xt {
            for y in yf...yt {
                for z in zf...zt {
                    space[z][y][x] = filler
                }
            }
        }
    }
}

// Straight forward counter
// A fold (+) on the complete array would give the same answer
func countCubesOn () -> Int {
    var cnt = 0
    for x in 0...sRange-1 {
        for y in 0...sRange-1 {
            for z in 0...sRange-1 {
                if space[z][y][x] == cubeON {
                    cnt += 1
                } 
            }
        }
    }
    return cnt
}


//  ------------- Main program -------------

print ("Advent of Code 2021 - day 22 - both parts in Swift")
// Start brute force cube lights switching
// Read the grid data
readAndParse()
// Simply count all the cubes that are lit ON
let ccOn = countCubesOn()
print ("After initialize, the number of cubes ON is: \(ccOn)")
print ("0K.\n")

