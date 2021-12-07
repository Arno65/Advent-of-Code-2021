-- Advent of Code 2021 - Day 7 task A & B
-- Solutions in Haskell
-- Also solving in other languages like Lisp
-- (Ter leering ende vermaeck...)
--
-- Determine the horizontal position that the crabs can align to using 
-- the least fuel possible.
-- The amount of fuel they spend to align to that position is: 349769
-- Now with the correct fuel consumption, 
-- as each crab moves, moving further becomes more expensive. 
-- The amount of fuel they spend to align to that position is: 99540554
--
--
-- (cl) by Arno Jacobs, 07-12-2021

-- 
module AoC2021d07ab where

import Data.List.Split  -- (splitOn)

part1 = 1 
part2 = 2

filename    = "data/inputDay07_2021.txt"
sComma      = ","

delta :: Int -> Int -> Int
delta = (\a -> \b -> abs (a - b))

calculateFuel :: Int -> Int -> [Int] -> Int
calculateFuel 1 sp cp = sum [ (delta sp ix) * pc     |  (pc,ix) <- zip cp [0..]]
calculateFuel _ sp cp = sum [ pc * div (d1*(d1+1)) 2 |  (pc,ix) <- zip cp [0..],
                                                        let d1 = delta sp ix ]

-- Minimum fuel use for part 1 or part 2
minimumFuel :: Int -> [Int] -> Int
minimumFuel part cp = minimum [ calculateFuel part sp cp | sp <- [0..length cp-1]]

-- Parsing for all numbers from 0 to the maximum of the list
tally :: [Int] -> [Int]
tally cp = [ length (filter (==ix) cp) | ix <- [0..maximum cp] ]

-- This way the 'read' has an output-type
readInts :: String -> Int
readInts = read

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 7 - both parts in Haskell"
            day7 <- tally <$> map readInts <$> splitOn "," <$> head <$> lines <$> readFile filename

            putStr   "Calculated by rules part 1, the least amount of fuel spend is: "
            print $ minimumFuel part1 day7

            putStr   "Calculated by rules part 2, the least amount of fuel spend is: "
            print $ minimumFuel part2 day7
            putStrLn "0K.\n"


