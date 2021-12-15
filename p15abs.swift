///     Advent of Code 2021 - Day 15 task A & B 
///     But with Extened GRID as input
///
///     Solutions in Swift
///     Also solved in Haskel
///     (Ter leering ende vermaeck...)
///
///
///     The lowest total risk path for part 1 is:  592
///     The lowest total risk path for part 2 is: 2897
///     
///
///     (cl) by Arno Jacobs, 15-12-2021

import Cocoa

let filename = "Code/Advent/AoC_2021/data/inputDay15_2021_extended.txt"

let overflow    = 999999999

let gCntP1 = 100
let gCount = 500    // This is also the maximum grid size: 500x500
var gRange = gCount - 1

var grid: [[Int]] = Array(repeating: Array(repeating: 0, count: gCount), count: gCount) 
var nxtg: [[Int]] = Array(repeating: Array(repeating: overflow, count: gCount), count: gCount)

// The read function
func readGrid () {
    if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
    {
        let fileURL = dir.appendingPathComponent(filename)
        // This will insert a dataset with a maximum size of 500x500 grid points
        // into a 500x500 Int-array 'grid[y][x]'
        do {
            // Read data
            let readGrid = try String(contentsOf: fileURL, encoding: .utf8)
            var x = 0
            var y = 0
            for c in readGrid {             // process character by character 
                if let i = Int(String(c)) {
                    grid[y][x] = i
                    if x < gRange { x += 1 }
                    else {  x = 0
                            y += 1
                    }
                }
            }
        }
        catch { print ("Read error?") }
    }
}

// Get from next-grid
func getGrid (x: Int, y: Int) -> Int {
    var rv = overflow
    if x >= 0 && x <= gRange && y >= 0 && y <= gRange {
        rv = nxtg[y][x]     // (x,y) in range
    }
    return rv
}

func nextRun () -> Bool {
    var px          = 0
    var py          = 0
    var preTop      = 0
    var preLeft     = 0
    var preBottom   = 0
    var preRight    = 0
    var minadd      = 0
    var newgv       = 0

    // SECOND RUN ---------------------------------------------------------
    nxtg[0][0] = 0
    var changed = false

    // First diagonal top left half - including centre diagonal
    for dc in 1...gRange {
        for px in 0...dc {
            py = dc - px
            preTop      = getGrid(x: px, y: py-1)
            preLeft     = getGrid(x: px-1, y: py)
            preBottom   = getGrid(x: px, y: py+1)
            preRight    = getGrid(x: px+1, y: py)
            minadd          = min(min(preTop,preLeft),min(preBottom,preRight))
            newgv           = grid[py][px] + minadd
            if nxtg[py][px] > newgv { changed = true }
            nxtg[py][px]    = newgv
        }
    }

    // Second diagonal bottom right
    for dc in 1...gRange {
        px = gRange
        for py in dc...gRange {
            preTop      = getGrid(x: px, y: py-1)
            preLeft     = getGrid(x: px-1, y: py)
            preBottom   = getGrid(x: px, y: py+1)
            preRight    = getGrid(x: px+1, y: py)
            minadd          = min(min(preTop,preLeft),min(preBottom,preRight))
            newgv           = grid[py][px] + minadd
            if nxtg[py][px] > newgv { changed = true }
            nxtg[py][px]    = newgv
            px -= 1
        }
    }

    return changed
}

//  ------------- Main program -------------  -------------  ------------- 
//

// Start path finder
print ("Advent of Code 2021 - day 15 - both parts in Swift")
readGrid()

// The 'grid' is loaded, now work until all short paths are found...
// For part 1 (a 100x100 grid)
gRange = gCntP1 - 1
while nextRun() { 
    // Run as long as path is changed...
} 
print ("The lowest total risk path for part 1 is:  \(nxtg[gRange][gRange])")

// Now part 2 (a 500x500 grid)
gRange = gCount - 1
while nextRun() { 
    // Run as long as path is changed...
} 
print ("The lowest total risk path for part 2 is: \(nxtg[gRange][gRange])")
print ("0K.\n")

