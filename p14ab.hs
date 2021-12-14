-- Advent of Code 2021 - Day 14 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- After 10 steps, # most common minus # least common element is: 2745
-- After 40 steps, # most common minus # least common element is: 3420801168962
--
--
-- (cl) by Arno Jacobs, 14-12-2021
 
module AoC2021d14ab where

import Data.List.Split  -- (splitOn)

filename = "data/inputDay14_2021.txt"

sSplit  = " -> "
part1N  = 10 :: Int
part2N  = 40 :: Int

-- Initial functions
parse :: String -> (String,String)
parse line = (p1,p2) where (p1:p2:_) = splitOn sSplit line

initPairs :: String -> [(String,Int)]
initPairs = spectrum . intoPairs
    where 
        intoPairs []        = []
        intoPairs (c:[])    = []
        intoPairs pps       = [take 2 pps] ++ intoPairs (tail pps)
        spectrum []         = []
        spectrum (pair:rps) = [(pair,pc)] ++ spectrum nextPairs
            where
                pc          = 1 + length (filter (==pair) rps)
                nextPairs   = filter (/=pair) rps

--
-- The real work
-- Grow polymer by rules in 'pairs'
growPolymer :: Int -> [(String,Int)] -> [(String,String)] -> [(String,Int)]
growPolymer 0   spectrum _      = spectrum
growPolymer cnt spectrum pairs  = growPolymer (cnt-1) newSpectrum pairs
    where newSpectrum = growPolymerOnce spectrum [] pairs

growPolymerOnce :: [(String,Int)] -> [(String,Int)] -> [(String,String)] -> [(String,Int)]
growPolymerOnce []             newSpectrum _     = newSpectrum 
growPolymerOnce (sp:rspectrum) newSpectrum pairs = growPolymerOnce rspectrum nsp2 pairs
    where
        pc              = snd sp 
        (newP1,newP2)   = newPair (fst sp) pairs
        nsp1            = addToSpectrum newSpectrum newP1 pc
        nsp2            = addToSpectrum nsp1        newP2 pc
        newPair sp pairs = ([p1] ++ inPart, inPart ++ [p2])
            where 
                inPart      = snd (head (filter (\(p,_) -> p==sp) pairs))
                (p1:p2:_)   = sp
        addToSpectrum spectrum pair cnt 
            | elem pair (map fst spectrum)  = rspectrum ++ [(pair,newCnt)]
            | otherwise                     =  spectrum ++ [(pair,   cnt)]
                where
                    rspectrum   = filter (\(p,_) -> p/=pair) spectrum
                    newCnt      = cnt + snd (head (filter (\(p,_) -> p==pair) spectrum))

--
-- The Final Countdown... uh... element counting
numberMostMinusLeast :: Char -> [(String,Int)] -> Int
numberMostMinusLeast lastElement polymerSP = maximum spc - minimum spc
    where 
        spc = map snd $ countSingleElements lastElement polymerSP
        countSingleElements lastElement polymerSP = 
            sumElements [(lastElement,1)] (splitElements polymerSP)
            where
                splitElements []                = []
                splitElements (((e:_),c):rpl)   = [(e,c)] ++ splitElements rpl
                sumElements sl    []             = sl
                sumElements subsl ((chr,cnt):rl)    
                    | elem chr (map fst subsl)  = sumElements nextSubsl rl
                    | otherwise                 = sumElements (subsl ++ [(chr,cnt)]) rl
                        where
                            newCnt      = cnt + snd (head (filter (\(c,_) -> c==chr) subsl))
                            nextSubsl   = [(chr,newCnt)] ++ (filter (\(c,_) -> c/=chr) subsl)
        
main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 14 - both parts in Haskell"
            day14 <- lines <$> readFile filename
            -- Create a spectrum for the current (input) polymer
            let spectrum    = initPairs $ head day14
            -- The counting of the elements the last element of the last pair is not counted
            -- Need to add 1 for that element to the counter
            let lastElement = (fst $ last spectrum) !! 1 
            -- The pairs that make the fill (or grow) rules
            let pairs       = map parse $ drop 2 day14

            let polymer1SP = growPolymer part1N spectrum pairs
            putStr "After " 
            putStr $ show part1N
            putStr " steps, # most common minus # least common element is: "
            print $ numberMostMinusLeast lastElement polymer1SP

            let polymer2SP = growPolymer part2N spectrum pairs
            putStr "After " 
            putStr $ show part2N
            putStr " steps, # most common minus # least common element is: "
            print $ numberMostMinusLeast lastElement polymer2SP

            putStrLn "0K.\n"
