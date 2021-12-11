-- Advent of Code 2021 - Day 11 task A & B
-- Solutions in Haskell
-- Also solving in other languages like ...
-- (Ter leering ende vermaeck...)
--
-- The total flashes after 100 steps is:               1735
-- The first step during which all octopuses flash is: 400
--
-- The next steps during which all octopuses flash is: 410, 420, 430...
-- From all 0 to all flashing in 10 steps, again and again.
-- Strange situation... 
-- Why wheren't the Octo's already synchronised flashing?
--

-- (cl) by Arno Jacobs, 11-12-2021
 
-- 
module AoC2021d11ab where

type Point              = (Int,Int)
type Cavern             = [[Int]]
type CavernFlashCount   = (Cavern,Int)

filename    = "data/inputDay11_2021.txt"

resetEnergy =   0 :: Int
flashing    =  10 :: Int    -- one bigger than 9
flashed     =  11 :: Int
border      =  -1 :: Int
steps1      = 100 :: Int
neighbours  = [ (-1,-1), (0,-1), (1,-1),
                (-1, 0),         (1, 0),
                (-1, 1), (0, 1), (1, 1)] :: [Point] 

-- Parse the input file
-- String "012345657" -> [0,1,2,3,4,5,6,7]
parseDigits :: String -> [Int]
parseDigits = map charToInt
    where charToInt c = read [c]

-- The 'complicated' rules for increasing energy levels and flashing...
-- https://adventofcode.com/2021/day/11

increaseOne :: Cavern -> Cavern
increaseOne = map (map (\x -> x+1))

flashOctopusStep ::  Cavern -> Cavern
flashOctopusStep = map (map resetFlash) 
    where 
        resetFlash octopus = if octopus >= flashing then resetEnergy else octopus

getEnergyLevelSafe :: Point -> Point -> Cavern -> Int
getEnergyLevelSafe (x,y) (mxx,mxy) cavern 
    | x < 0 || x > mxx || y < 0 || y > mxy  = border
    | otherwise                             = cavern !! y !! x  

getNeighbouringFlashes:: Int -> Point -> Point -> Cavern -> Int
getNeighbouringFlashes cel (x,y) maxxy cavern
    | cel >= flashing   = 0     -- The octopus is already flashing
    | otherwise         = 
        length [ flashing |    
            (nx,ny) <- neighbours, 
            let nel = getEnergyLevelSafe (x+nx,y+ny) maxxy cavern,
            nel == flashing ]
                                
workEnergyLevelsStep :: Cavern -> Cavern
workEnergyLevelsStep cavern =
    [[ newOcto
        |   x <- [0..mxx], 
            let elo  = getEnergyLevelSafe (x,y) (mxx,mxy) cavern,
            let nelc = getNeighbouringFlashes elo (x,y) (mxx,mxy) cavern,
            let newOcto = max elo (min flashing (elo + nelc)) ]
        |   y <- [0..mxy] ]
        where 
            mxx = length (head cavern) - 1
            mxy = length cavern - 1

resetFlashed :: Cavern -> Cavern -> Cavern
resetFlashed cavern previousCavern =
    [[ updated
        |   x <- [0..mxx], 
            let octo    = getEnergyLevelSafe (x,y) (mxx,mxy) cavern,
            let preOcto = getEnergyLevelSafe (x,y) (mxx,mxy) previousCavern,
            let updated = if preOcto == flashing then flashed else octo ]
        |   y <- [0..mxy] ]
        where 
            mxx = length (head cavern) - 1
            mxy = length cavern - 1

workEnergyLevelsSteps :: Cavern -> Cavern
workEnergyLevelsSteps cavern
    | cntNewFlashes == 0    = flashOctopusStep iterCavern
    | otherwise             = workEnergyLevelsSteps iterCavern
    where
        prepCavern          = workEnergyLevelsStep cavern
        iterCavern          = resetFlashed prepCavern cavern
        cntNewFlashes       = length $ filter isFlashing $ concat iterCavern
        isFlashing octopus  = octopus == flashing

workFlashesStep ::  CavernFlashCount -> CavernFlashCount
workFlashesStep (cavern,flashCount) = (nextStep,newFlashCount)
    where
        nextStep            = workEnergyLevelsSteps $ increaseOne cavern
        newFlashCount       = flashCount + cntNewFlashed
        cntNewFlashed       = length $ filter hasFlashed $ concat nextStep
        hasFlashed octopus  = octopus == resetEnergy
        
workFlashesSteps :: Int -> CavernFlashCount -> CavernFlashCount
workFlashesSteps 0 (cavern,flashCount) = (cavern,flashCount)
workFlashesSteps n (cavern,flashCount) = workFlashesSteps (n-1) nextStep
    where
        nextStep = workFlashesStep (cavern,flashCount)

flashesAfterSteps ::  Int -> Cavern -> Int
flashesAfterSteps steps cavern = snd $ workFlashesSteps steps (cavern,0)

allFlashing :: Cavern -> Int
allFlashing = allFlashing' 1
    where
        allFlashing' step cavern    
            | allFlashed    = step
            | otherwise     = allFlashing' (step+1) nextStep
                where
                    (nextStep,_)    = workFlashesStep (cavern,0)
                    allFlashed      = (sum (concat nextStep)) == resetEnergy

main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 11 - both parts in Haskell"
            day11 <- map parseDigits <$> lines <$> readFile filename
            putStr "The total flashes after "
            putStr $ show $ steps1
            putStr " steps is:               "
            print $ flashesAfterSteps steps1 day11 
            putStr "The first step during which all octopuses flash is: "
            print $ allFlashing day11 
            putStrLn "0K.\n"

