-- Advent of Code 2021 - Day 3 task A & B
-- Solutions in Haskell
-- Also solving in other languages like ... 
-- (Ter leering ende vermaeck...)
--
-- The power consumption of the submarine: 841526
-- The life support rating of the submarine: 4790390
--
-- (cl) by Arno Jacobs, 03-12-2021

-- 
module AoC2021d03ab where

filename = "data/inputDay03_2021.txt"

-- helpers
parse :: String -> [Int]
parse = map (\b -> if b == '1' then 1 else 0)

fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits base = foldl (\x -> \y -> base*x+y) 0 

-- Swap the individual bit from 1 to 0 and vise versa
-- b.t.w. Bits 0 and 1 are 'stored' type Int 
swapBits :: [Int] -> [Int]
swapBits = map (\x -> 1-x) 

-- main code for part 1
--
countCommons :: Num a => [[a]] -> [a]
countCommons [[]]   = []
countCommons wll    = countCommons' wll empty
    where 
        empty = take (length (head wll)) $ repeat 0
     -- countCommons' :: Num a => [[a]] -> [a] -> [a]
        countCommons' []       cl = cl
        countCommons' (bl:rbl) cl = countCommons' rbl [ c+b | (b,c) <- zip bl cl ]

toCommons :: [[Int]] -> [Int]
toCommons bl = [ (if (b*2 >= mx) then 1 else 0 )| b <- countCommons bl ]
    where mx  = length bl 

calculatePowerConsumption :: [[Int]] -> Int
calculatePowerConsumption bl = fromDigits 2 gamma * fromDigits 2 epsilon
    where           
        gamma   = toCommons bl
        epsilon = swapBits gamma


-- Code for part 2

select :: Eq a => [[a]] -> [a] -> Int -> [[a]]
select bl cl bp = filter (\l -> l !! bp == bit) bl
    where bit = cl !! bp

recToCommons :: [[Int]] -> [Int] -> Int -> Bool -> [Int]
recToCommons bl cl bp inv   | length sl == 1    = head sl
                            | otherwise         = recToCommons sl ncl (bp+1) inv
    where
        sl  = select bl cl bp
        hcl = toCommons sl 
        ncl = if inv then swapBits hcl else hcl

calculateLifeSupportRating :: [[Int]] -> Int
calculateLifeSupportRating bl = fromDigits 2 oxygen * fromDigits 2 co2
    where
        gamma   = toCommons bl      -- again
        epsilon = swapBits gamma    -- and again...
        oxygen  = recToCommons bl gamma   0 False
        co2     = recToCommons bl epsilon 0 True

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 3 - both parts in Haskell"
            day3 <- map parse <$> lines <$> readFile filename             
            putStr "The  power consumption  of the submarine: "
            print $ calculatePowerConsumption day3
            putStr "The life support rating of the submarine: "
            print $ calculateLifeSupportRating day3
            putStrLn "0K.\n"

