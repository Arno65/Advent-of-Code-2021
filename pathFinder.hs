-- For Advent of Code 2021 - Day 12 task A & B a "create graph function" is needed
-- This code will be a more 'generic' version, for any given set of links
-- An input line has to look like
-- "AB-xy" or "from -> to" or "1,2" or "celA to celB"
-- The seperator for splitting has to be a String.
-- So "From -> to" will be split by " -> " and result in: ("From","to")
-- The Start and End point have to be identified in the code.
--
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--

-- (cl) by Arno Jacobs, 13-12-2021
 
-- 
module PathFinder where

import Data.List.Split (splitOn)

-- Type 'a' will need Eq and Show. 
-- In this verion 'String' is used.
type Link  a = (a,a)
type Links a = [Link a]
type Path  a = (a, [a])
type Paths a = [Path a]

filename = "../data/inputDay12_2021.txt"

-- Split string, start and end point strings.
sSplit  = "-" 
sStart  = "start"
sEnd    = "end"

-- Here the input lines are split into a 'from' part and a 'to' part (per line).
parse :: Eq a => [a] -> [a] -> ([a], [a])
parse splitStr line = (s1,s2) where (s1:s2:_) = splitOn splitStr line

-- Graph creation
graph :: Eq a => Links a -> Paths a
graph = graph' []
    where 
        graph' currentPath []               = currentPath
        graph' currentPath (link:restLinks) = graph' newPath restLinks
            where 
                newPath = addToPath (addToPath currentPath link) (swap link)
                swap    = \(a,b) -> (b,a)

addToPath :: Eq a => Paths a -> Link a -> Paths a
addToPath cp (p1,p2)
    | cp1 == []  = cp ++ [(p1,[p2])]
    | otherwise  = ncp
        where
            cp1  = filter (\(c,_) -> c == p1) cp
            ppt1 = [p2] ++ snd (head cp1)
            cpr1 = filter (\(c,_) -> c /= p1) cp
            ncp  = cpr1 ++ [(p1,ppt1)]

-- Print list -> one element each line
printList :: Show a => [a] -> IO ()
printList []     = do  return ()
printList (l:ls) = do  print l
                       printList ls

main :: IO ()
main = do   putStrLn "Path finder in Haskell\n"
            dataset <- lines <$> readFile filename
            let links = map (parse sSplit) dataset
            let gd = graph links

            putStrLn "The input file with all the links:"
            print links
            putStrLn "\nThe processed links into graph data:"
            printList gd
            putStrLn "\n0K.\n"
