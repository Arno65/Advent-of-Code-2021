-- Advent of Code 2021 - Day 1 task A & B
-- Solutions in Haskell
-- Also solving in other languages like Lisp, Rust, Swift and (old school) BASIC
-- (Ter leering ende vermaeck...)
--
-- The count of increased measurements is: 1692
-- The count of increased triplets is: 1724
--
-- (cl) by Arno Jacobs, 01-12-2021

-- 
module AoC2021d01ab where

filename = "data/inputDay01_2021.txt"

-- The lines marked with "-- (#)" are not needed for this quest.

-- This first version is a recursive counter
countIncreases' :: [Int] -> Int
countIncreases' []          = 0     -- (#)
countIncreases' (_:[])      = 0
countIncreases' (m1:m2:ms)  | m2 > m1   = 1 +   countIncreases' (m2:ms)
                            | otherwise =       countIncreases' (m2:ms)
                          
-- With a 'zip' the list and the tail of the list are zipped
-- This creates pairs of elements and its succesor element 
-- Like: [1,2,3,4,5] -> [(1,2),(2,3),(3,4),(4,5)]
-- With a 'map' the pairs are tested creating a list of '1' for increase and '0' for not
-- The 'sum' will count the increased measurements
countIncreases'' :: [Int] -> Int
countIncreases'' xs = sum $ map testIncrease $ zip xs $ tail xs
    where testIncrease (x,y) = if y>x then 1 else 0

-- Here is a version with a 'foldl'
-- If speed is an issue than a fold is much faster than a recursive version
-- If the list is long than lost of memory is needed for the zip of two long lists
-- The 'foldl' will fold the list into one value.
countIncreases :: [Int] -> Int
countIncreases (x:xs) = snd $ foldl testIncrease (x,0) xs
    where testIncrease (a,c) b = if b > a then (b,c+1) else (b,c)

sumTriplets :: [Int] -> [Int]
sumTriplets []             = []     -- (#)
sumTriplets (_:[])         = []     -- (#)
sumTriplets (_:_:[])       = []
sumTriplets (m1:m2:m3:ms)  = [m1+m2+m3] ++ sumTriplets (m2:m3:ms)

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 1 - both parts in Haskell"
            day1 <- map read <$> lines <$> readFile filename
            putStr   "The count of increased measurements is: "
            print $ countIncreases day1
            putStr   "The count of increased triplets is: "
            print $ countIncreases $ sumTriplets day1
            putStrLn "0K.\n"

            