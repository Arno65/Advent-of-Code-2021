-- Advent of Code 2021 - Day 9 task A & B
-- Solutions in Haskell
-- Also solving in other languages like Lisp
-- (Ter leering ende vermaeck...)
--
-- The sum of the risk levels of all low points on your heightmap is: 591
-- The product of the sizes of the three largest basins is: 1113424
--

-- (cl) by Arno Jacobs, 09-12-2021
 
-- 
module AoC2021d09ab where

import Data.List    -- (sort)

type Point = (Int,Int)

filename    = "data/inputDay09_2021.txt"
border      = 9 :: Int
nLargest    = 3 :: Int  -- Quest part 2: Find the three (3) largest basins
neighbours  = [(-1,0),(1,0),(0,-1),(0,1)] :: [Point] 

-- Parse the input file
-- String "012345657" -> [0,1,2,3,4,5,6,7]
parseHeightmap :: String -> [Int]
parseHeightmap = map charToInt
    where charToInt c = read [c]

-- quick sort but only store unique elements
-- So: [2,4,3,1,1,2,3,5,2,6,3,4] -> [1,2,3,4,5,6]
-- This way the list is sorted and all elements are unique
usort :: Ord a => [a] -> [a]
usort []     = []
usort (e:rl) = usort smaller ++ [e] ++ usort bigger
                where
                    smaller = filter (<e) rl
                    bigger  = filter (>e) rl 


-- Part 1
--
-- (no checking for empty list)
findLowpoints :: [[Int]] -> [(Int,(Int,Int))]
findLowpoints lpl = [  (lpl !! y !! x, (x,y)) |    
                            x <- [fst minp..fst maxp], 
                            y <- [snd minp..snd maxp],
                            lpl !! y !! x < border, -- 9 is never a low point
                            checkLowpoint (x,y) minp maxp lpl ]
    where 
        -- total grid range
        minp = (0,0)   
        maxp = (length (head lpl) - 1, length lpl - 1)

-- Checking for a low point (x,y)
checkLowpoint :: Point -> Point -> Point -> [[Int]] -> Bool
checkLowpoint (x,y) minp maxp hl =
        -- get and check height points of 8 neighbouring positions (border safe)
        and [ llcp < getSafeHeightPoint (x+dx,y+dy) minp maxp hl |   
                (dx,dy) <- neighbours ]
            where llcp = hl !! y !! x

-- Regarding borders, safe read neighbouring points
-- Set border at height '9' -- also being the edge of a basin 
getSafeHeightPoint :: Point -> Point -> Point -> [[Int]] -> Int
getSafeHeightPoint (cx,cy) (mix,miy) (mxx,mxy) hl
    | cx >= mix && cx <= mxx && cy >= miy && cy <= mxy  = hl !! cy !! cx
    | otherwise                                         = border -- set border points high

-- Simply add 1 to the height of a low point
calculateRisklevels :: [(Int,Point)] -> [Int]
calculateRisklevels = map ((\x -> x+1) . fst)

-- Part 2
--
setBorder :: [[Int]] -> Point -> [[Int]]
setBorder hl (x,y) = 
    take y hl ++
    [take x row ++ [border] ++ drop (x+1) row] ++
    drop (y+1) hl
        where row = hl !! y

getBasins :: [[Int]] -> [(Int,Point)] -> [[Point]]
getBasins hl lpl = map (getBasin minp maxp hl []) lowPoints
    where 
        lowPoints = map snd lpl
        minp = (0,0)
        maxp = (length (head hl) - 1, length hl - 1)

-- This was a bottle neck
-- I added the 'visited' list -- this is the list of basin points
-- The four different directions are scanned seperatly and 
-- get the previous visited list 
getBasin minp maxp hl visited (x,y) 
    | nhp == border         = []
    | elem (x,y) visited    = []
    | otherwise             = visited'
        where 
            nhp         = getSafeHeightPoint (x,y) minp maxp hl
            hl'         = setBorder hl (x,y)
            nvl         = [(x,y)] ++ visited
            nxtl1       = getBasin minp maxp hl' nvl (x - 1,y)
            vl1         = nxtl1 ++ nvl
            nxtl2       = getBasin minp maxp hl' vl1 (x,y - 1)
            vl2         = nxtl2 ++ vl1
            nxtl3       = getBasin minp maxp hl' vl2 (x + 1,y)
            vl3         = nxtl3 ++ vl2
            nxtl4       = getBasin minp maxp hl' vl3 (x,y + 1)
            visited'    = usort $ nxtl4 ++ vl3
            
productOfLargestBasins :: Int -> [[(Int,Int)]] -> Int
productOfLargestBasins n = product . take n . reverse . sort . map length

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 9 - both parts in Haskell"
            day9 <- map parseHeightmap <$> lines <$> readFile filename
            let lowPoints = findLowpoints day9
            putStr "The sum of the risk levels of all low points on your heightmap is: "
            print $ sum $ calculateRisklevels lowPoints

            let basins = getBasins day9 lowPoints
            putStr "The product of the sizes of the three largest basins is:           "
            print $ productOfLargestBasins nLargest basins
            putStrLn "0K.\n"

