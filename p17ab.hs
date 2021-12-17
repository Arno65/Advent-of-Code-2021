-- Advent of Code 2021 - Day 17 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The highest Y position reached on this trajectory is:       4753
-- The number of hits for distinct initial velocity values is: 1546
--
--
-- input: target area: x=137..171, y=-98..-73

-- (cl) by Arno Jacobs, 17-12-2021
 
-- module AoC2021d17ab where

data Object = StartPoint | Target | Probe   deriving Eq
data Hit    = Hit | Miss                    deriving Eq

type Coordinate = (Int,Int)
type Point      = (Coordinate,Object)
type Points     = [Point]

--- Initialize start positions -------------------------------------------------
--
start_point = ((0,0),StartPoint) :: Point

target_area_X = [137..171] :: [Int]
target_area_Y = [-98..(-73)] :: [Int]

targetPoints :: Points
targetPoints = [((x,y),Target) | x <- target_area_X, y <- target_area_Y ]

start_grid :: Points
start_grid = [start_point] ++ targetPoints

--- Calculate trajectories -------------------------------------------------------
--
calculateTrajectory :: Coordinate -> Points -> (Points,Hit,Int)
calculateTrajectory vxy points = 
    calculateTrajectory' (fst start_point) vxy mxxy points
        where
            crdn = map fst points
            -- So it's maximum X (far right from start)
            -- and     minimum Y (deep down under targets)
            mxxy = ( maximum $ map fst crdn, minimum $ map snd crdn)
        
calculateTrajectory' :: Coordinate -> Coordinate -> Coordinate -> Points -> (Points,Hit,Int)
calculateTrajectory' (px,py) (vx,vy) (mx,my) points 
    | hit               = ( nPoints, Hit,  maxY)
    | miss1 || miss2    = ( nPoints, Miss, maxY)
    | otherwise         = calculateTrajectory' (nPx,nPy) (nVx,nVy) (mx,my) nPoints
        where
            nPx = px + vx
            nPy = py + vy
            hit = [] /= filter (\(x,y) -> x==nPx && y==nPy) (map fst targetPoints)
            nVx = vx - if vx == 0 then 0 else 1 -- no fixed acceleration on x-axis
            nVy = vy - 1                        -- fixed acceleration on y-axis
            nPoints = [((nPx,nPy),Probe)] ++ points
            maxY    = maximum $ map snd (map fst points)
            miss1   = nPx > mx || nPy < my      -- Passed target without hit
            miss2   = vx == 0 &&                -- Vx == 0 and wil NEVER hit target 
                      ( px < head target_area_X ||
                        px > last target_area_X )
            

--   --  Part 1  --  -- 
--

-- This functions 'scans' the Y's for given X's 
-- Return either a 1-element list if there is a 'top-hit'
--        or an empty list
--
heuristicFindYs :: Coordinate -> [(Coordinate,Int)]
heuristicFindYs (x,maxY) = filter (\(_,ty) -> ty == topY) hitList
    where 
        hitList = heuristicFindYs' x maxY (x,x) 
        topY    = maximum $ map snd hitList
    --  heuristicFindYs' :: Int -> Int -> Coordinate -> [(Coordinate,Int)]
        heuristicFindYs' prevTopY maxY (x,y) 
            | y >= maxY             = rlv   -- In case of misses over target             
            | prevTopY < newTopY    = rlv ++ heuristicFindYs' newTopY maxY (x,y+1)
            | otherwise             = rlv 
                where 
                    (_,hit,newTopY) = calculateTrajectory (x,y) start_grid
                    -- We only need the hits
                    rlv = if hit == Hit then [((x,y),newTopY)] else []
            
heuristicTopShot :: (Coordinate,Int)
heuristicTopShot = head $ filter (\(_,mx) -> mx == maxY) scan
    where
        maxY = maximum $ map snd scan
        scan = concat [ heuristicFindYs (x, head target_area_X) | x <- [alpha..beta] ]
            where
                -- Sum (n = 1 to N) {n} = N(N+1)/2  so in target range... 
                alpha   = isqrt2 $ head target_area_X
                beta    = isqrt2 $ last target_area_X
                isqrt2  = floor . sqrt . fromIntegral . (\x -> 2 * x)

--   --  Part 2  --  -- 
--
-- A bit more clever
-- First part is ALL targetpoints, these are ALL direct hits at the first step 
-- Next all directions just above the target area upto maximum of maxY
-- maxY is the maximum top level calculated in Part 1
--
getAllInitialValues :: Int -> [Coordinate]
getAllInitialValues maxY =
    map fst targetPoints ++
    [ (x,y) |   x <- [1..head target_area_X - 1],
                y <- [(1 + last target_area_Y)..maxY],
                let (_,hit,_) = calculateTrajectory (x,y) start_grid,
                hit == Hit ]


main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 17 - both parts in Haskell"
            putStr "The highest Y position reached on this trajectory is:       "
            let ((_,maxY),top) = heuristicTopShot
            print top
            putStr "The number of hits for distinct initial velocity values is: "
            print $ length $ getAllInitialValues maxY
            putStrLn "0K.\n"

