-- Advent of Code 2021 - Day 21 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The final score for the the losing player is:      929625
-- The number of universes for the winning player is: 175731756652760
--
-- (cl) by Arno Jacobs, 21-12-2021
 
-- 
module AoC2021d21ab where

-- The next via "cabal install memoize"
import Data.Function.Memoize -- ('memoize2' for many worlds universe - part 2)

-- Player 1 starting position: 6
-- Player 2 starting position: 1
pPos1 = 6
pPos2 = 1

-- game data
nThrows         = 3
boardSize       = 10
winningScore1   = 1000
winningScore2   = 21
dice1           = [1..100]
dice2           = [1,2,3]

-- Part 1
--
-- The 'deterministic dice'
diceThrows :: [Int]
diceThrows = concat $ repeat dice1

-- Take the 3 rolls from the 'deterministic dice'
-- The minus '1' after the sum is because (later on)
-- of the mod function with output range [1..Modulo]
rollDice :: [Int] -> (Int,[Int])
rollDice rolls = (sum (take nThrows rolls) - 1, drop nThrows rolls)

playTheGame :: Int
playTheGame = snd . head . filter (\(s,_) -> s >= winningScore1) $ 
                        playTheGame' (pPos1,0) (pPos2,0) diceThrows 0
    where
        playTheGame' (pp1,sp1) (pp2,sp2) nextDiracDice round    -- endless...
            =   [( sp2 + np2, nThrows *  round    * sp1 )] ++ 
                [( sp1 + np1, nThrows * (round+1) * sp2 )] ++ 
                playTheGame' (np1,sp1+np1) (np2,sp2+np2) ndd2 (round+2)
            where
                (throws1,ndd1)  = rollDice nextDiracDice
                (throws2,ndd2)  = rollDice ndd1
                np1             = 1 + mod (pp1 + throws1) boardSize
                np2             = 1 + mod (pp2 + throws2) boardSize
        
-- Part 2
--
playAllUniverses :: Int
playAllUniverses = uncurry max $ search (pPos1,0) (pPos2,0)

search :: (Int, Int) -> (Int, Int) -> (Int, Int)
search playerPosScore1 playerPosScore2 = search' worldSplit (0,0) playerPosScore1 playerPosScore2
    where 
        worldSplit = [ x+y+z | x <- dice2, y <- dice2, z <- dice2 ]        
        search' []      (w12)   _       _       = (w12)
        search' (s:rws) (w1,w2) (p1,s1) (p2,s2) 
            | s1' >= winningScore2  = search' rws ( w1+1,  w2    ) (p1,s1) (p2,s2) 
            | otherwise             = search' rws ( w1+r1, w2+r2 ) (p1,s1) (p2,s2) 
            where
                p1' = 1 + mod (p1 + s - 1) 10                   -- board position [1..10]
                s1' = s1 + p1'
                -- 'memoize2' at the rigt place (otherwise we need most of all the time there is...)
                (r2,r1) = memoize2 search (p2,s2) (p1',s1')      -- many worlds... recursive
            

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 21 - both parts in Haskell"            
            putStr "The final score for the the losing player is:      "
            print playTheGame
            putStr "The number of universes for the winning player is: "
            print playAllUniverses  -- even with 'memoize2' it's still slow in 'ghci'
            putStrLn "0K.\n"

