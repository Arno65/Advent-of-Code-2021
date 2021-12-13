-- Advent of Code 2021 - Day 13 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of points visible after one fold is: 755
-- The code visible after all the folds is:        BLKJRBAG
--
--

-- (cl) by Arno Jacobs, 13-12-2021
 
-- 
module AoC2021d13ab where

import Data.List.Split  -- (splitOn)

type Point = (Int,Int)

filename = "data/inputDay13_2021.txt"

sSplit          = ","
cSplit          = head sSplit
cFoldVertical   = 'x'          
cFoldHorizotal  = 'y'
cIs             = '='
cEmpty          = ' '
sPlot           = "#"

-- Some helpers
--
-- quick sort but only store unique elements
-- So: [3,5,2,4,3,1,1,2,3,5,2,6,3,4] -> [1,2,3,4,5,6]
usort :: Ord a => [a] -> [a]
usort []     = []
usort (e:rl) = usort smaller ++ [e] ++ usort bigger
    where
        smaller = filter (<e) rl
        bigger  = filter (>e) rl 

-- Print list of strings 
printStrings :: [String] -> IO ()
printStrings []     = do return ()
printStrings (l:ls) = do putStrLn l
                         printStrings ls

-- Parse from input strings to two seperate lists of points 
parse :: [String] -> ([Point],[Point])
parse = parse' ([],[])
    where
        parse' dataset        []            = dataset
        parse' (points,folds) (line:rlines)     
            | elem cSplit line          = parse' (nps,folds)                    rlines
            | elem cFoldHorizotal line  = parse' (points,folds ++ [(0,foldnr)]) rlines
            | elem cFoldVertical  line  = parse' (points,folds ++ [(foldnr,0)]) rlines
            | otherwise                 = parse' (points,folds)                 rlines
                where
                    (sx:sy:_)   = splitOn sSplit line
                    nps         = points ++ [(read sx, read sy)]
                    foldnr      = read $ tail $ dropWhile (/=cIs) line

-- Functions for a 1x fold (horizontal or vertical)
foldPaper1x :: [Point] -> Point -> [Point]
foldPaper1x points (fx,fy)  | fx == 0   = foldHorizontal fy points
                            | otherwise = foldVertical   fx points

-- The one time horizontal fold
foldHorizontal :: Int -> [Point] -> [Point]
foldHorizontal fy points =
    usort [ (x,ny )| (x,y) <- points, let ny = if y > fy then 2*fy-y else y ]

-- The one time vertical fold
foldVertical :: Int -> [Point] -> [Point]
foldVertical fx points = 
    usort [ (nx,y )| (x,y) <- points, let nx = if x > fx then 2*fx-x else x ]

-- In real life a sheet of A4 printer paper can be folded only (about) 7 times
-- In 2001, high school student Britney Gallivan of Pomona, California, 
-- successfully managed to fold a paper in half 12 times by using a roll of long, 
-- thin specialty toilet paper that was 1.2 kilometers in length. 
-- (https://www.thesprucecrafts.com/number-times-you-can-fold-paper-2540658)
-- In this digital world the boundary is not on the physics of paper.
-- So let's fold & fold...
-- For this task I need 12 folds. 0K.
foldPaper :: [Point] -> [Point] -> [Point]
foldPaper points []             = points
foldPaper points (fold:rfolds)  = foldPaper (foldPaper1x points fold) rfolds

-- After folding is done this function wil plot a '#' for every point
-- In this task a eight capital letter code is shown
makePlotString :: [Point] -> [String]
makePlotString points = insertPoints points lines
    where
        (mx,my) = ( maximum $ map fst points, maximum $ map snd points )
        lines   = take (my+1) ( repeat (take (mx+1) $ repeat cEmpty))
        insertPoints []     lines = lines
        insertPoints (p:ps) lines = insertPoints ps (insertPoint p lines)
        insertPoint (x,y) lines = take y lines ++
                                  [take x yline ++ sPlot ++ drop (x+1) yline] ++ 
                                  drop (y+1) lines
            where yline = lines !! y

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 13 - both parts in Haskell"
            day13 <- lines <$> readFile filename
            let (points,folds)  = parse day13
            putStr "The number of points visible after one fold is: "
            print $ length $ foldPaper1x points (folds !! 0)
            putStrLn "The code visible after all the folds is: "
            let plotStr = makePlotString $ foldPaper points folds
            printStrings plotStr
            putStrLn "0K.\n"

