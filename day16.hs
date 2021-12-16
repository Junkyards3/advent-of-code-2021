{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as M
import qualified Data.Bifunctor
import Data.Char (digitToInt)
import Data.Foldable (Foldable(foldl'))

type Version = Int
type Type = Int
data Packet = Literal (Int,Version,Type) | Operator ([Packet],Version,Type) deriving (Show)

main :: IO ()
main = do
    inputFile <- openFile "input/input_day16.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;
    let (p,n) = parse $ hexPacketToBin fileContent
    print $ versionSum p ;
    putStrLn "Result for part 2 : " ;
    print $ evalPacket p ;

hexPacketToBin :: String -> String
hexPacketToBin = concatMap (M.fromList [('0',"0000"),('1',"0001"),('2',"0010"),('3',"0011"),('4',"0100"),('5',"0101"),('6',"0110"),
    ('7',"0111"),('8',"1000"),('9',"1001"),('A',"1010"),('B',"1011"),('C',"1100"),('D',"1101"),('E',"1110"),('F',"1111")] M.!)

binStrToInt :: String -> Int
binStrToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0

versionSum :: Packet -> Int
versionSum (Literal (l,v,t)) = v
versionSum (Operator (lp,v,t)) = v + sum (map versionSum lp)

evalPacket :: Packet -> Int 
evalPacket (Literal (l,v,t)) = l 
evalPacket (Operator (lp,v,t))
    | t == 0 = sum lev 
    | t == 1 = product lev 
    | t == 2 = minimum lev
    | t == 3 = maximum lev 
    | t == 5 = fromEnum $ head lev > lev !! 1 
    | t == 6 = fromEnum $ head lev < lev !! 1 
    | otherwise = fromEnum $ head lev == lev !! 1  
        where lev = map evalPacket lp

-- second component is the number of characters actually used in the parsing
parse :: String -> (Packet,Int)
parse (v1:v2:v3:t1:t2:t3:xs)
    | packetType == 4 = (\(v,m) -> (Literal (v,version,packetType),m)) $ parseLiteral xs 0
    | otherwise = (\(lp,m) -> (Operator (lp,version,packetType),m)) $ parseOperator xs where
        packetType = binStrToInt [t1,t2,t3]
        version = binStrToInt [v1,v2,v3]

        parseLiteral :: String -> Int -> (Int,Int)
        parseLiteral (x1:x2:x3:x4:x5:ys) acc
            | x1 == '0' = (16*acc + binStrToInt [x2,x3,x4,x5],11)
            | otherwise = (\(v,m) -> (v,m+5)) $ parseLiteral ys (16*acc + binStrToInt [x2,x3,x4,x5])

        parseOperator :: String -> ([Packet],Int)
        parseOperator (i:ys)
            | i == '0' = (parseOperator0 lengthSub (drop 15 ys),22 + lengthSub)
            | otherwise = parseOperator1 nbSub (drop 11 ys)
            where lengthSub = binStrToInt $ take 15 ys
                  nbSub = binStrToInt $ take 11 ys

        parseOperator0 :: Int -> String -> [Packet]
        parseOperator0 0 _ = []
        parseOperator0 n s = p : parseOperator0 (n-m) (drop m s)
            where (p,m) = parse s

        parseOperator1 :: Int -> String -> ([Packet],Int)
        parseOperator1 0 _ = ([],18)
        parseOperator1 n s = Data.Bifunctor.bimap (p :) (m +) $ parseOperator1 (n-1) (drop m s)
            where (p,m) = parse s