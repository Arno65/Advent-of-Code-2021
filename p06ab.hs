-- Advent of Code 2021 - Day 6 task A & B
-- Solutions in Haskell
-- Also solving in other languages like Lisp
-- (Ter leering ende vermaeck...)
--
-- The number of lanternfish after  80 days is: 388739
-- The number of lanternfish after 256 days is: 1741362314973
--
--
-- (cl) by Arno Jacobs, 06-12-2021

-- 
module AoC2021d06ab where

import Data.List.Split  -- (splitOn)

filename    = "data/inputDay06_2021.txt"
sComma      = ","
nRuns1      = 80 
nRuns2      = 256 

-- The simple (recursive) brute force - creating huge list - version
-- This will only work for the first task 
-- This is NOT used for this task anymore
-- Only here to report my 'progress'... ;-)
lifeLanternFish :: Int -> [Int] -> Int
lifeLanternFish 0 lfl = length lfl
lifeLanternFish c lfl = lifeLanternFish (c-1) (genNext ++ genNew)
    where
        genTmp  = map (\x -> x-1) lfl
        cNew    = length $ filter (==endTimer) genTmp
        genNew  = take cNew $ repeat newTimer
        genNext = map (\x -> if x==endTimer then resetTimer else x) genTmp
        -- 
        endTimer    = (-1)
        resetTimer  = 6
        newTimer    = 8

-- The new function
-- Start with a count list for the digits [0..9]
-- (Although 9 is not used...)
-- First all 0's.
-- Read input and count digits.
-- Next iterate generations
-- And create 'fish' is 0's-count is larger than zero.
-- BTW. The code uses 'Integer'. It can handle very very big 'lists'... 
--
endlessLanternFish :: Int -> [Integer] -> Integer
endlessLanternFish 0 tcl = sum tcl
endlessLanternFish c tcl = endlessLanternFish (c-1) nextGen
    where
        nextCnt6 = tcl !! 0 + tcl !! 7
        nextCnt7 = tcl !! 8
        nextCnt8 = tcl !! 0 
        nextGen  = take 6 (tail tcl) ++ [nextCnt6, nextCnt7, nextCnt8]

-- Parsing for all digits, but 9 is not needed
-- '9' will not be used in the 'endlessLanternFish' counter
parse :: [Int] -> [Integer]
parse tl = [ fromIntegral (length (filter (==ix) tl)) | ix <- [0..9] ]

-- This way the 'read' has an output-type
readInts :: String -> Int
readInts = read

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 6 - both parts in Haskell"
            day6 <- parse <$> map readInts <$> splitOn "," <$> head <$> lines <$> readFile filename
            putStr   "The number of lanternfish after  "
            putStr $ show nRuns1
            putStr   " days is: "
            print $ endlessLanternFish nRuns1 day6
            putStr   "The number of lanternfish after "
            putStr $ show nRuns2
            putStr   " days is: "
            print $ endlessLanternFish nRuns2 day6
            putStrLn "0K.\n"

-- The input set - for testing and debugging
hl :: [Int]
hl = [  2,1,1,1,1,1,1,5,1,1,1,1,5,1,1,3,5,1,1,3,1,1,3,1,4,4,4,5,1,1,1,3,1,
        3,1,1,2,2,1,1,1,5,1,1,1,5,2,5,1,1,2,1,3,3,5,1,1,4,1,1,3,1,1,1,1,1,
        1,1,1,1,1,1,1,4,1,5,1,2,1,1,1,1,5,1,1,1,1,1,5,1,1,1,4,5,1,1,3,4,1,
        1,1,3,5,1,1,1,2,1,1,4,1,4,1,2,1,1,2,1,5,1,1,1,5,1,2,2,1,1,1,5,1,2,
        3,1,1,1,5,3,2,1,1,3,1,1,3,1,3,1,1,1,5,1,1,1,1,1,1,1,3,1,1,1,1,3,1,
        1,4,1,1,3,2,1,2,1,1,2,2,1,2,1,1,1,4,1,2,4,1,1,4,4,1,1,1,1,1,4,1,1,
        1,2,1,1,2,1,5,1,1,1,1,1,5,1,3,1,1,2,3,4,4,1,1,1,3,2,4,4,1,1,3,5,1,
        1,1,1,4,1,1,1,1,1,5,3,1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        5,1,1,1,1,1,1,1,1,5,1,4,4,1,1,1,1,1,1,1,1,3,1,3,1,4,1,1,2,2,2,1,1,
        2,1,1]

-- the parsed input set
pl :: [Integer]
pl = [0,200,27,25,23,25,0,0,0,0]
