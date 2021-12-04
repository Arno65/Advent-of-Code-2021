-- Advent of Code 2021 - Day 4 task A & B
-- Solutions in Haskell
-- Also solving in other languages like ... 
-- (Ter leering ende vermaeck...)
--
-- The final score of the first bingo: 71708
-- The score of the last bingo:        34726
--
-- (cl) by Arno Jacobs, 04-12-2021

-- 
module AoC2021d04ab where

import Data.List        -- (transpose)
import Data.List.Split  -- (splitOn)

filename = "data/inputDay04_2021.txt"

parseNumbers :: String -> String -> [Int]
parseNumbers sc = map read . filter (/="") . splitOn sc 

parseCards :: [String] -> [[[Int]]]
parseCards []       = []
parseCards (l:ls)   | l == ""   = parseCards ls
                    | otherwise = [parseOneCard (l:ls)] ++ parseCards (dropWhile (/="") ls)
    where 
        parseOneCard []     = []
        parseOneCard (l:ls) | l == ""   = []
                            | otherwise = [parseNumbers " " l] ++ parseOneCard ls

playBingo :: [Int] -> [([[Int]],[[Int]])] -> Int
playBingo (n:ns) cards  | hasBingo newCards = scoreBingo n newCards
                        | otherwise         = playBingo ns newCards
    where newCards = eraseNumber n cards
    
eraseNumber :: Int -> [([[Int]],[[Int]])] -> [([[Int]],[[Int]])] 
eraseNumber _ []           = []
eraseNumber n ((ci,ct):cs) = [(nci,nct)] ++ eraseNumber n cs
    where
        nci = eraseNumber' n ci
        nct = eraseNumber' n ct
        eraseNumber' n cn = map (filter (/=n)) cn

hasBingo :: [([[Int]],[[Int]])] -> Bool
hasBingo []             = False
hasBingo ((ci,ct):cs)   | hasEmpty ci || hasEmpty ct    = True
                        | otherwise                     = hasBingo cs

hasEmpty :: [[Int]] -> Bool
hasEmpty []         = False
hasEmpty (il:ils)   | il == []  = True
                    | otherwise = hasEmpty ils

scoreBingo :: Int -> [([[Int]],[[Int]])] -> Int
scoreBingo n ((ci,ct):cs)   | hasEmpty ci || hasEmpty ct    = n * sumCard ci
                            | otherwise                     = scoreBingo n cs
    where sumCard = sum . map sum

-- Erase all cards that have a Bingo
eraseCard :: [([[Int]],[[Int]])] -> [([[Int]],[[Int]])] 
eraseCard []            = []
eraseCard ((ci,ct):cs)  | hasEmpty ci || hasEmpty ct    = eraseCard cs
                        | otherwise                     = [(ci,ct)] ++ eraseCard cs

-- Only play the last bingo card
playLastCard :: [Int] -> [([[Int]],[[Int]])] -> Int
playLastCard (n:ns) cards   | length cards == 1 = playBingo (n:ns) cards    
                            | otherwise         = playLastCard ns nxtCards
    where nxtCards = eraseCard $ eraseNumber n cards

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 4 - both parts in Haskell"
            day4 <- lines <$> readFile filename   
            let numbers = parseNumbers "," (head day4)
            let icards  = parseCards (drop 2 day4)
            let cards   = zip icards (map transpose icards)
            let firstsc = playBingo    numbers cards   
            let lastsc  = playLastCard numbers cards

            putStr "The final score of the first bingo: "
            print firstsc
            putStr "The score of the last bingo:        "
            print lastsc 
            putStrLn "0K.\n"

