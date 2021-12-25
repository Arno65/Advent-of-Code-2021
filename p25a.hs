-- Advent of Code 2021 - Day 25 task A
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The first step the sea cucumbers don't move is: 432
--

-- (cl) by Arno Jacobs, 25-12-2021
 
-- module AoC2021d25a where

import Data.List    -- (transpose)

data CucumberDirection = East | South | None deriving Eq

type Sealine    = [CucumberDirection]
type Seabed     = [Sealine]

-- Characters for parsing
east    = '>'
south   = 'v'

filename = "data/inputDay25_2021.txt"

parse :: [String] -> Seabed
parse = map (map parseLocation)
    where
        parseLocation '>'   = East     
        parseLocation 'v'   = South
        parseLocation _     = None

workLine :: CucumberDirection -> Sealine -> Sealine
workLine cucumdir sealine
    | go_round  = [cucumdir] ++ workLine' cucumdir [] slRound ++ [None]
    | otherwise =               workLine' cucumdir [] sealine
        where
            p1          = head sealine
            go_round    = p1 == None && last sealine == cucumdir
            slRound     = tail $ init sealine

workLine' :: CucumberDirection -> Sealine -> Sealine -> Sealine
workLine' cucumdir nsl []               = nsl
workLine' cucumdir nsl (sl1:[])         = nsl ++ [sl1]
workLine' cucumdir nsl (sl1:sl2:rsl)
    | sl1 == cucumdir && sl2 == None    = workLine' cucumdir (nsl ++ [sl2,sl1]) rsl
    | otherwise                         = workLine' cucumdir (nsl ++ [sl1])     (sl2:rsl) 

workOneStep :: Seabed -> Seabed
workOneStep = transpose . map (workLine South) . transpose . map (workLine East)

countStepsTillFixedPoint :: Seabed -> Int -> Int
countStepsTillFixedPoint seabed steps
    | seabed == nextSeabed  = steps
    | otherwise             = countStepsTillFixedPoint nextSeabed (steps+1)
        where nextSeabed = workOneStep seabed


main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 25 - solution in Haskell"
            seabed <- parse <$> lines <$> readFile filename
            putStr "The first step the sea cucumbers don't move is: "
            print $ countStepsTillFixedPoint seabed 1            
            putStrLn "0K.\n"
            
