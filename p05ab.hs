-- Advent of Code 2021 - Day 5 task A & B
-- Solutions in Haskell
-- Also solving in other languages like ... 
-- (Ter leering ende vermaeck...)
--
-- Considering only horizontal and vertical lines, 
-- the number of points where at least two lines overlap is: 6007
-- Considering horizontal, vertical and only diagonal lines at 45 degrees, 
-- the number of points where at least two lines overlap is: 19349
--
--
-- (cl) by Arno Jacobs, 05-12-2021

-- 
module AoC2021d05ab where

import Data.List        -- (sort)
import Data.List.Split  -- (splitOn)

filename = "data/inputDay05_2021.txt"
sarrow   = " -> "
sxy      = ","

nDangerVents = 2 :: Int

type Point = (Int,Int)
type Line  = (Point,Point)

-- From String :: "1,2 -> 3,4" to Line :: ((1,2),(3,4))
parseCoordinates :: String -> Line
parseCoordinates sc = ((read x1,read y1),(read x2,read y2))
    where (x1:y1:x2:y2:_) = concat $ map (splitOn sxy) (splitOn sarrow sc)

-- The next three functions could be combined in one
-- That would speed up the process with a factor of three
-- But I coded them seperately for easy debuging
-- The first tasks went all 0K
-- I did have issues with the challenging 'drawd45' for the second task 

-- Horizontal line -- y1 == y2
drawHorizontal :: [Line] -> [Point]
drawHorizontal []                       = []
drawHorizontal (((x1,y1),(x2,y2)):ls)   
    | y1 == y2  = [ (x,y1) | x <- [min x1 x2..max x1 x2]] ++ drawHorizontal ls  -- if horizontal, draw that line
    | otherwise = drawHorizontal ls

-- Vertical line -- x1 == x2
drawVertical :: [Line] -> [Point]
drawVertical []                         = []
drawVertical (((x1,y1),(x2,y2)):ls)     
    | x1 == x2  = [ (x1,y) | y <- [min y1 y2..max y1 y2]] ++ drawVertical ls    -- if vertical, draw that line
    | otherwise = drawVertical ls

-- Diagonal line at 45 degrees -- dx == dy
drawd45 :: [Line] -> [Point]
drawd45 []                         = []
drawd45 (((x1,y1),(x2,y2)):ls)     
    | dx == dy  = [ ( spx+s, spy + df*s ) | s <- [0..dx]] ++ drawd45 ls -- if 45 degrees, draw that line
    | otherwise = drawd45 ls
        where
            dx  = max x1 x2 - min x1 x2                             -- delta x and y coordinates
            dy  = max y1 y2 - min y1 y2
            ((spx,spy),(epx,epy)) = startPoint ((x1,y1),(x2,y2))    -- first point seen from x-axis
            df  = signum $ epy - spy                                -- rise or falling line 
            -- 
            startPoint ((x1,y1),(x2,y2))    
                | x1 < x2   = ((x1,y1),(x2,y2))
                | otherwise = ((x2,y2),(x1,y1))       

-- Collecting all horizontal and vertical lines
drawHorizontalVertical :: [Line] -> [Point]
drawHorizontalVertical lines = sort $ drawVertical lines ++ drawHorizontal lines

-- Collecting all horizontal, vertical and diagonal lines at 45 degrees
drawHVD45 :: [Line] -> [Point]
drawHVD45 lines = sort $ drawVertical lines ++ drawHorizontal lines ++ drawd45 lines

-- count every point on the grid with 'ovl' number or more overlapping points 
countDangerPoints :: Int -> [Point] -> Int
countDangerPoints _   []        = 0
countDangerPoints ovl (p:ps)    | length cpl > ovl-2 = 1 + countDangerPoints ovl nxtl
                                | otherwise          =     countDangerPoints ovl nxtl
    where
        cpl  = takeWhile (==p) ps -- if any, take rest of overlapping points 
        nxtl = dropWhile (==p) ps -- if any, remove overlapping points


main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 5 - both parts in Haskell"
            day5 <- map parseCoordinates <$> lines <$> readFile filename   

            putStrLn "Considering only horizontal and vertical lines,"
            putStr   "the number of points where at least two lines overlap is: "
            print $ countDangerPoints 2 $ drawHorizontalVertical day5
            putStrLn "Considering horizontal, vertical and only diagonal lines at 45 degrees,"
            putStr   "the number of points where at least two lines overlap is: "
            print $ countDangerPoints 2 $ drawHVD45 day5
            putStrLn "0K.\n"

