-- Advent of Code 2021 - Day 20 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of lit pixels after  2 in the resulting image is: 5044
-- The number of lit pixels after 50 in the resulting image is: 18074
--
--

-- (cl) by Arno Jacobs, 20-12-2021
 
-- module AoC2021d20ab where

type Line   = [Int]
type Grid   = [Line]
type Point  = (Int,Int)
type Pixel  = Int

filename = "data/inputDay20_2021.txt"

part1 = 2
part2 = 50

pixelOff    = 0 :: Pixel
pixelOn     = 1 :: Pixel
cEmpty      = '.'
cPlot       = '#'
sCR         = "\n"


-- Parse from input strings to a lists of points for all and only all lit points
-- 
parse :: [String] -> Grid
parse = map (map (\pc -> if pc == cPlot then pixelOn else pixelOff))

-- With base 2, binary to decimal conversion
fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits base = foldl (\x -> \y -> base*x+y) 0 

-- Don't need range checking - workGrid will NOT ask for values near the edges
indexValuePixel :: Point -> Grid -> Int
indexValuePixel (x,y) g = fromDigits 2 [ g !! (y+dy) !! (x+dx) | dy <- [-1..1], dx <- [-1..1]]

--
workGrid :: Int -> Line -> Grid -> Grid
workGrid step a g = [[ a !! (indexValuePixel (x,y) eg) | x <- [1..mx-1]] | y <- [1..my-1]]
    where
        -- First some testing for infinity and beyond...
        fill = if (a !! 0 == 0) || (mod step 2 == 0) then pixelOff else pixelOn
        eg   = expandGrid fill g 
        mx   = length (head eg) - 1
        my   = length eg - 1
        
-- Expand the grid with TWO 'pixel' lit border
expandGrid :: Pixel -> Grid -> Grid
expandGrid pixel g = el ++ el ++ map expandLine g ++ el ++ el
    where 
        el           = [take (length (head g)+4) (repeat pixel)]
        expandLine l = [pixel,pixel] ++ l ++ [pixel,pixel]

-- Iterate, incl. step count info for infinity control
solve :: Int -> Line -> Grid -> Grid
solve 0   _ g = g
solve cnt a g = solve (cnt-1) a ng
    where ng = workGrid cnt a g

-- All the lis pixels have value '1' and the rest is '0'
countLitPixels :: Grid -> Int
countLitPixels = sum . concat 

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 20 - both parts in Haskell"
            day20 <- lines <$> readFile filename
            let algorithm = head $ parse $ take 1 day20
            let grid  = parse (drop 2 day20)
            putStr "The number of lit pixels after  "
            putStr $ show part1
            putStr " in the resulting image is: "
            print $ countLitPixels $ solve part1 algorithm grid            
            putStr "The number of lit pixels after "
            putStr $ show part2
            putStr " in the resulting image is: "
            print $ countLitPixels $ solve part2 algorithm grid            
            putStrLn "0K.\n"
            
            