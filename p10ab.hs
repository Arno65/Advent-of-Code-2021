-- Advent of Code 2021 - Day 10 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total syntax error score for corrupt strings is:     464991
-- The middle syntax error score for incomplete strings is: 3662008566
--

-- (cl) by Arno Jacobs, 10-12-2021
 
-- 
module AoC2021d10ab where

import Data.List        -- (sort)

filename    = "data/inputDay10_2021.txt" :: String
openChunks  = "([{<" :: String   
closeChunks = ")]}>" :: String

nCorrupt        =  (-1) :: Int
foo             = (-42) :: Int
noIx            = [('.',-1)] :: [(Char,Int)]

--
-- Score for   )  ]    }     >
scoreTab1   = [3,57,1197,25137] :: [Int]
scoreTab2   = [1, 2,   3,    4] :: [Int]
scoreFact2  = 5 :: Int

-- Part 1
--
-- Get the score for a corrupted navigation subsystem line
getScoreCorruptedLine :: String -> Int
getScoreCorruptedLine ecs   
    | sl == []              = 0
    | head sl == nCorrupt   = scoreTab1 !! (sl !! 1)
    | otherwise             = 0 
        where sl = parseChunksLine [] ecs

parseChunksLine :: [Int] -> String -> [Int]
parseChunksLine locs []         = locs
parseChunksLine locs (cc:ccs)
                                -- skip 'non-parsing' characters
    | not cCheck                = parseChunksLine locs ccs
    | ixoc >= 0                 = parseChunksLine (ixoc:locs) ccs
    | ixcc >= 0 && loc /= ixcc  = (nCorrupt:ixcc:[])
    | ixcc >= 0                 = parseChunksLine (tail locs) ccs
                                -- fummy - filler -  will not apply...
    | otherwise                 = [nCorrupt,foo]    
        where 
            -- Check for 'legal' parse characters
            cCheck  = elem cc $ openChunks ++ closeChunks 
            loc     = head locs
            ixoc    = getIndex openChunks  cc
            ixcc    = getIndex closeChunks cc

getIndex :: String -> Char -> Int
getIndex s c = snd $ head $ (filter (\(sp,_) -> sp == c) (zip s [0..])) ++ noIx


-- Part 2
--
-- Score for Incomplete - per line
getScoreIncomplete :: String -> Int
getScoreIncomplete ecs  | sl == []              = 0
                        | head sl /= nCorrupt   = scoreIncompletes sl
                        | otherwise             = 0
    where sl = parseChunksLine [] ecs
            
scoreIncompletes :: [Int] -> Int
scoreIncompletes sl = foldl (\x -> \y -> scoreFact2 * x + scoreTab2 !! y) 0 sl

-- Index pf list starts at 0
-- div odd 2 is always rounded downwards 
-- -> so this will produce index of middle value of list     
middleScoreIncomplete :: [String] -> Int
middleScoreIncomplete ecss = sts !! (div (length sts) 2)
    where sts = sort $ filter (/= 0) $ map getScoreIncomplete ecss
        
main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 10 - both parts in Haskell"
            day10 <- lines <$> readFile filename
            putStr "The total syntax error score for corrupt strings is:     "
            print $ sum $ map getScoreCorruptedLine day10
            putStr "The middle syntax error score for incomplete strings is: "
            print $ middleScoreIncomplete day10
            putStrLn "0K.\n"

