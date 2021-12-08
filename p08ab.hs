-- Advent of Code 2021 - Day 8 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- In the output values, digit count for 1, 4, 7, or 8 is: 512
-- After deduction, the sum of all the output values is: 1091165
--

-- (cl) by Arno Jacobs, 08-12-2021
 
module AoC2021d08ab where

import Data.List        -- (sort)
import Data.List.Split  -- (splitOn)

data Match = DoMatch | NoMatch | ReverseMatch
        deriving (Eq,Show)

filename    = "data/inputDay08_2021.txt"
sc1         = "|"
sc2         = " "

checkDigits                 = [1,4,7,8] :: [Int]    -- digits with unique number of segments
checkDigitsSegmentsCount    = [2,4,3,7] :: [Int]    -- numbers of segments for 1,4,7, and 8
segmentsCount               = [6,2,5,5,4,5,6,3,7,6] :: [Int]


-- String "abc bc de | a b c" ->  (["abc","bc","de"],["a","b","c"])
parseDigits :: String -> ([String],[String])
parseDigits dl = (  map sort $ noEmpties $ splitOn sc2 (tdl !! 0), 
                    map sort $ noEmpties $ splitOn sc2 (tdl !! 1))
    where 
        tdl = splitOn sc1 dl
        noEmpties = filter (/="")

-- Part 1
-- There are unique number of segments for 1,4,7,8 being 2, 4, 3 and 7
-- Look for segments with length 2,3,4 or 7 and count them
countOnSegments :: [Int] -> [String] -> Int
countOnSegments cd ovls = length [ 1 | ovl <- ovls, elem (length ovl) cd ]

countDigits :: [Int] -> [([String],[String])] -> Int
countDigits cd cdl = sum $ map (countOnSegments cd) ovdl
    where ovdl = map snd cdl

-- Part 2
--
-- 0K.
-- This is not a generic solution
-- It will match the segments to the display digits for this task
-- If the pattern of a digits is changed this will NOT compute
--
-- Being so... this works and is efficient... i.m.h.o. ;-)
-- Here is where the real work is done -- match all segments
-- We know that 1,4,7 and 8 are correct
-- We know that 2,3 and 5 have 5 segments   -- 3! is 6 permutations to check
-- We know that 0,6 and 9 have 6 segments   -- 3! is 6 permutations to check
-- 
finalOrdening :: [Int] -> [String] -> [String]
finalOrdening scs ol1 = [   segments0, segments1, segments2, segments3, segments4, 
                            segments5, segments6, segments7, segments8, segments9 ]
    where
        fiveSegments = [ol1 !! 2] ++ [ol1 !! 3] ++ [ol1 !! 5]
        sixSegments  = [ol1 !! 0] ++ [ol1 !! 6] ++ [ol1 !! 9]    
        segments1 = ol1 !! 1
        segments4 = ol1 !! 4
        segments7 = ol1 !! 7
        segments8 = ol1 !! 8
        -- first check - segments for '3'
        segments3       = getMatch DoMatch fiveSegments segments1
        fiveSegments'   = filter (/= segments3) fiveSegments
        -- first check - segments for '6'
        segments6       = getMatch NoMatch sixSegments segments1
        sixSegments'    = filter (/= segments6) sixSegments
        -- first check - segments for '5'
        segments5       = getMatch ReverseMatch fiveSegments' segments6
        -- first check - segments for '2' -- remaining of 5 segments (not 3 or 5)
        segments2       = head $ filter (/= segments5) fiveSegments'
        -- first check - segments for '9'
        segments9       = getMatch DoMatch sixSegments' segments3
        -- first check - segments for '0' -- remaining of 6 segments (not 6 or 9)
        segments0       = head $ filter (/= segments9) sixSegments'

-- Look for certain segment combinations based on known segment combinations
-- This can be done by a match, a non-match or an reversed match
--
getMatch :: Match -> [String] -> String -> String
getMatch DoMatch segments cs = head [ sgm | sgm <- segments,       doesMatch sgm cs ]
getMatch NoMatch segments cs = head [ sgm | sgm <- segments, not $ doesMatch sgm cs ]
getMatch _       segments cs = head [ sgm | sgm <- segments,       doesMatch cs sgm ]    

-- A segment "bcd" will match cs "bd" but NOT "cg"
-- A segment "bcd" will reverse match "abcde" but NOT "abce"
doesMatch :: String -> String -> Bool
doesMatch segment cs = and [ elem c segment | c <- cs ]   

-- This will order the unique digits and furthermore digits on the number of segments
firstOrdening :: [Int] -> [String] -> [String]
firstOrdening [] _          = []
firstOrdening (sc:scs) sl   = [fds] ++ firstOrdening scs nsl
    where
        fds = head $ filter (\ds -> sc == length ds) sl
        nsl = filter (/= fds) sl

-- Here the ordening of digits - per line - is started
-- First a first ordening (1,4,7,8), next the 'search' for (3,6,5,2,9,0)
orderDigitList :: [Int] -> [String] -> [(String,Int)]
orderDigitList sc sl = zip fol [0..]
    where fol = finalOrdening sc $ firstOrdening sc sl

-- From list of digits to Int for given base
-- fromDigits 10 [1,2,3]            -> 123
-- fromDigits 2 [1,1,1,1,0,1,1]     -> 123
-- fromDigits 8  [1,7,3]            -> 123
--
fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits base = foldl (\x -> \y -> base*x+y) 0 

-- With the ordered segments the digit number is matched to the segments
-- The list of digit numbers is calculated into an Int (like: [4,2] -> 42)
matchDigits :: [String] -> [(String,Int)] -> Int
matchDigits ovls dl = fromDigits 10 
                        [ snd $ head $ dn |    
                            ovd <- ovls, 
                            let dn = filter (\(ds,_) -> ds==ovd) dl]

-- All the matching per line is done and all the numbers are summed
sumAllDigits :: [Int] -> [([String],[String])] -> Int
sumAllDigits sc cdl = sum [ matchDigits ovls dl | (ovls,dl) <- zip ovdlss odll ]
    where 
        ovdlss  = map snd cdl
        odll    = map (orderDigitList sc) (map fst cdl)

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 8 - both parts in Haskell"
            day8 <- map parseDigits <$> lines <$> readFile filename
            putStr "In the output values, digit count for 1,4,7 or 8 is:  "
            print $ countDigits checkDigitsSegmentsCount day8
            putStr "After deduction, the sum of all the output values is: "
            print $ sumAllDigits segmentsCount day8
            putStrLn "0K.\n"

