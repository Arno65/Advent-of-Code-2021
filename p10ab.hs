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
nCorrupt         =  (-1) :: Int
foo              = (-42) :: Int
--
-- Score for           )  ]    }     >
scoreTablePart1     = [3,57,1197,25137] :: [Int]
scoreTablePart2     = [1, 2,   3,    4] :: [Int]
scoreFactorPart2    = 5 :: Int

-- Part 1
--
-- Get the score for a corrupted navigation subsystem line
getScoreCorrupted :: String -> Int
getScoreCorrupted ecs   | check == nCorrupt = score
                        | otherwise         = 0 
    where 
        (_,sl) = parseScoreChunksLine ecs
        check  = head sl
        score  = scoreTablePart1 !! (sl !! 1)

parseScoreChunksLine :: String -> ([Int],[Int])
parseScoreChunksLine = nparseChunksLine [0,0,0,0] []

nparseChunksLine :: [Int] -> [Int] -> String -> ([Int],[Int])
nparseChunksLine il locs [] = (il,locs)
nparseChunksLine (i1:i2:i3:i4:_) locs (cc:ccs)
    | cc == openChunks  !! 0    = nparseChunksLine (i1+1:i2:i3:i4:[]) (0:locs) ccs
    | cc == openChunks  !! 1    = nparseChunksLine (i1:i2+1:i3:i4:[]) (1:locs) ccs
    | cc == openChunks  !! 2    = nparseChunksLine (i1:i2:i3+1:i4:[]) (2:locs) ccs
    | cc == openChunks  !! 3    = nparseChunksLine (i1:i2:i3:i4+1:[]) (3:locs) ccs
    | cc == closeChunks !! 0 && loc /= 0    = ([i1,i2,i3,i4],(nCorrupt:0:[]))
    | cc == closeChunks !! 1 && loc /= 1    = ([i1,i2,i3,i4],(nCorrupt:1:[]))
    | cc == closeChunks !! 2 && loc /= 2    = ([i1,i2,i3,i4],(nCorrupt:2:[]))
    | cc == closeChunks !! 3 && loc /= 3    = ([i1,i2,i3,i4],(nCorrupt:3:[]))
    | cc == closeChunks !! 0    = nparseChunksLine (i1-1:i2:i3:i4:[]) nlocs ccs
    | cc == closeChunks !! 1    = nparseChunksLine (i1:i2-1:i3:i4:[]) nlocs ccs
    | cc == closeChunks !! 2    = nparseChunksLine (i1:i2:i3-1:i4:[]) nlocs ccs
    | cc == closeChunks !! 3    = nparseChunksLine (i1:i2:i3:i4-1:[]) nlocs ccs
    | otherwise                 = ([foo],[nCorrupt,foo])    -- filler... will not apply
    where 
        loc     = head locs
        nlocs   = tail locs


-- Part 2
--
-- Score for Incomplete - per line
getScoreIncomplete :: String -> Int
getScoreIncomplete ecs  | check /= nCorrupt = score
                        | otherwise         = 0
    where
        (_,sl)  = parseScoreChunksLine ecs
        check   = head sl
        score   = scorePart2 sl
            
scorePart2 :: [Int] -> Int
scorePart2 sl = foldl (\x -> \y -> scoreFactorPart2 * x + scoreTablePart2 !! y) 0 sl


-- Index pf list starts at 0
-- div odd 2 is always rounded downwards 
-- -> so this will produce index of middle value of list     
middleScoreIncomplete :: [String] -> Int
middleScoreIncomplete ecss = sts !! (div tln 2)
    where
        sts = sort $ filter (/= 0) $ map getScoreIncomplete ecss
        tln = length sts
        

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 10 - both parts in Haskell"
            day10 <-  lines <$> readFile filename
            putStr "The total syntax error score for corrupt strings is:     "
            print $ sum $ map getScoreCorrupted day10
            putStr "The middle syntax error score for incomplete strings is: "
            print $ middleScoreIncomplete day10
            putStrLn "0K.\n"
