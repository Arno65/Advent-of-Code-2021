-- Advent of Code 2021 - Day 2 task A & B
-- Solutions in Haskell
-- Also solving in other languages like Lisp, Rust and Swift
-- (Ter leering ende vermaeck...)
--
-- With the first set of rules the result if you multiply the final 
-- horizontal position by the final depth is: 1762050
-- With the second set of rules the result if you multiply the final 
-- horizontal position by the final depth is: 1855892637
--
-- (cl) by Arno Jacobs, 02-12-2021

-- 
module AoC2021d02ab where

import Data.List.Split -- (splitOn)

filename = "data/inputDay02_2021.txt"

-- forward | up | down
data Direction a = Forward a | Up a | Down a deriving Show

type Position   = (Int,Int)
type Target     = (Position,Int)

cForward = 'f' :: Char
cUp      = 'u' :: Char
cDown    = 'd' :: Char
sSpace   = " " :: String

start1 = (0,0)      :: Position
start2 = ((0,0),0)  :: Target


productPair :: (Int,Int) -> Int
productPair (x,y) = x*y

parse :: String -> Direction Int
parse (x:xs)    | x == cForward = Forward rv 
                | x == cUp      = Up rv   
                | x == cDown    = Down rv     
    where rv = read $ (splitOn sSpace xs) !! 1

tripOne :: [Direction Int] -> Position -> Position
tripOne []             cp      = cp
tripOne (Forward x:ds) (ch,cv) = tripOne ds (ch+x, cv)
tripOne (Up x:ds)      (ch,cv) = tripOne ds (ch, cv-x)
tripOne (Down x:ds)    (ch,cv) = tripOne ds (ch, cv+x)

tripTwo :: [Direction Int] -> Target -> Position
tripTwo []             (cp,_)           = cp
tripTwo (Forward x:ds) ((ch,cv),aim)    = tripTwo ds ((ch+x, cv+(aim*x)),aim)
tripTwo (Up x:ds)      (cp,aim)         = tripTwo ds (cp,aim-x)
tripTwo (Down x:ds)    (cp,aim)         = tripTwo ds (cp,aim+x)


main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 2 - both parts in Haskell"
            day2 <- map parse <$> lines <$> readFile filename
            putStr   "With the first rules the position of the submarine after the trip is: "
            let np1 = tripOne day2 start1
            print np1
            putStr   "The product of the end position after task one is: "
            print $ productPair np1
            
            putStr   "With the new rules the position of the submarine after the trip is: "
            let np2 = tripTwo day2 start2
            print np2
            putStr   "The product of the end position after task two is: "
            print $ productPair np2
            
            putStrLn "0K.\n"
