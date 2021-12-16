-- Advent of Code 2021 - Day 16 task A & B
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The sum of the version numbers in all packets is: 901
-- The result of the evaluation of all packets is:   110434737925
--

-- (cl) by Arno Jacobs, 16-12-2021
 
{-# LANGUAGE NamedFieldPuns #-}
--    This, so code like:
-- -> sum_pvn (Literal {version}:ps) = version
--    will compile.


module AoC2021d16ab where

import Data.Char

-- Data structure for working with 
-- Buoyancy Interchange Transmission System (BITS)
data Packet a = Literal  {  version :: a, 
                            value   :: a  } | 
                Operator {  version :: a, 
                            typeID  :: a, 
                            packets :: [Packet a] }
        deriving Show

type Bits = [Int]

filename            = "data/inputDay16_2021.txt"
total_length_ID     = 0 :: Int
n_sub_packets_ID    = 1 :: Int
literal_value_ID    = 4 :: Int

-- An list of 'Int' in base 'base' into a base 10 'Int'
-- So: fromDigits  2 [0,0,1,0,1] -> 5
fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits base = foldl (\x -> \y -> base*x+y) 0 

-- A HEX Character into a 4 bit array of 0's and 1's
cByteToBit :: Char -> Bits
cByteToBit b = toBinary 4 (digitToInt b)
    where 
        toBinary c n | c < 1     = []
                     | otherwise = toBinary (c-1) d ++ [m]
            where (d,m) = divMod n 2

-- Parse from input strings to two seperate lists of points 
decode :: String -> Bits
decode = concat . map cByteToBit 

-- A '1' marks the first 4 bits and waiting for the next
-- A '0' marks the last 4 bits
--  v    v    v   e   
-- '100011001000011010101' -> '0001' + '0010' + '0011' -> '000100100011' (291)
getLiteralValue :: Bits -> Bits
getLiteralValue bits    
    | head bits == 1    = lvp ++ getLiteralValue (drop 5 bits) 
    | otherwise         = lvp
        where lvp = take 4 $ tail bits

-- Process all bits, all packets
workPackets :: Bits -> [Packet Int]
workPackets bits | bits == []   = []
                 | otherwise    = newPacket ++ workPackets rBits
    where (newPacket,rBits) = workPacket bits

-- Process some bits for "one" packet, return that packet and the remaining bits 
workPacket :: Bits -> ([Packet Int],Bits)
workPacket bits    
    | rBits   == []                 = ([],[])
    | cTypeID == literal_value_ID   = ([rvLV],bitsLV)
    | lTID    == total_length_ID    = ([rvOP0],rBits0)
    | lTID    == n_sub_packets_ID   = ([rvOP1],rBits1)
    | otherwise                     = ([],[])
        where
            cVersion    = fromDigits 2 $ take 3 bits
            cTypeID     = fromDigits 2 $ take 3 $ drop 3 bits
            rBits       = drop 6 bits
            -- Literal Value (typeID == 4)
            lvBits  = getLiteralValue rBits
            lv      = fromDigits 2 lvBits
            dropVal = 5 * div (length lvBits) 4
            bitsLV  = drop dropVal rBits
            rvLV    = Literal  { version = cVersion, value = lv } 
            -- Operator (typeID /= 4)
            -- mode 0 - length type ID 
            -- total length in bits
            lTID    = head rBits
            -- One can take 16 here, first bit is '0'
            nBits0  = fromDigits 2 $ take 16 rBits 
            subPck0 = take nBits0 $ drop 16 rBits
            rBits0  = drop (16+nBits0) rBits  
            rvOP0   = Operator {    version = cVersion, 
                                    typeID  = cTypeID, 
                                    packets = workPackets subPck0 }
            -- Operator (typeID /= 4)
            -- mode 1 - length type ID 
            -- number of sub-packets immediately contained
            nsp             = fromDigits 2 $ take 11 $ tail rBits
            (wpNx1,rBits1)  = workPacketNtimes nsp (drop 12 rBits)
            rvOP1           = Operator {    version = cVersion,
                                            typeID  = cTypeID,
                                            packets = wpNx1 }

-- The operator (length type ID mode 1) has to process a number of
-- 'n' times a sub-packet
-- This function will work those n-x sub-packets            
workPacketNtimes :: Int -> Bits -> ([Packet Int],Bits)
workPacketNtimes 0 bits = ([],bits)
workPacketNtimes n bits = ( newPacket ++ nxtPacket, rBits2)
    where 
        (newPacket,rBits1) = workPacket bits
        (nxtPacket,rBits2) = workPacketNtimes (n-1) rBits1


-- For part 1 --- --- --- --- --- --- --- --- --- --- --- --- --- ---
--
-- Summing of all the version numbers in all the packets and sub-packets
sum_pvn :: [Packet Int] -> Int
sum_pvn []                                  = 0
sum_pvn (Literal  { version }:ps)           = version + sum_pvn ps
sum_pvn (Operator { version, packets }:ps)  = version + sum_pvn (packets ++ ps)
    

-- For part 2 --- --- --- --- --- --- --- --- --- --- --- --- --- ---
--
-- The evaluation of the expression represented by the hexadecimal-encoded 
-- BITS transmission.
-- BTW - not using a Maybe-value or monads
-- But ooooh soooo happy with pattern matching!
eval Literal  { value }                 = value
eval Operator { typeID = 0, packets }   = sum     $ map eval packets
eval Operator { typeID = 1, packets }   = product $ map eval packets
eval Operator { typeID = 2, packets }   = minimum $ map eval packets
eval Operator { typeID = 3, packets }   = maximum $ map eval packets
eval Operator { typeID = 5, packets }   = greater $ map eval packets
eval Operator { typeID = 6, packets }   = less    $ map eval packets
eval Operator { typeID = 7, packets }   = equal   $ map eval packets
eval _                                  = error "Unknown packet value!" 
-- That last line of code should not happen for todays task...

-- Helpers for 'eval'
greater (e1:e2:_) = if e1 >  e2 then 1 else 0
less    (e1:e2:_) = if e1 <  e2 then 1 else 0
equal   (e1:e2:_) = if e1 == e2 then 1 else 0


main :: IO ()
main = do   putStrLn "Advent of Code 2021 - day 16 - both parts in Haskell"
            day16 <- decode <$> readFile filename
            putStr "The sum of the version numbers in all packets is: "
            print $ sum_pvn $ workPackets day16
            putStr "The result of the evaluation of all packets is:   " 
            print $ eval $ head $ workPackets day16
            putStrLn "0K.\n"

