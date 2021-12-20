{-# LANGUAGE BangPatterns #-}
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Set as S
import Data.Array ( Array, listArray, (!) )
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Debug.Trace (trace)

main :: IO ()
main = do
    inputFile <- openFile "input/input_day20.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;
    let (iea,img) = process fileContent ;
    print $ S.size . snd $ foldl' (\imgp i -> applyStep imgp iea) img [1,2];
    putStrLn "Result for part 2 : " ;
    print $ S.size . snd $ foldl' (\imgp i -> applyStep imgp iea) img [1..50];

type IEA = (Bool,Array Int Bool) -- True is flipping is needed
type Image = (Bool,S.Set Pos) --True if the light pixels are represented, False otherwise
type Pos = (Integer,Integer)

process :: String -> (IEA,Image)
process s = ((iea ! 0,iea),(True,img)) where
    t = splitOn "\n\n" s
    iea = listArray (0,511) $ map (== '#') $ head t
    t2 = splitOn "\n" (t !! 1)
    n = toInteger $ length $ head t2
    img = fst $ foldl' (\(s, (x,y)) c -> (if c == '#' then (x,y) `S.insert` s else s,if x == n-1 then (0,y+1) else (x+1,y)) )
        (S.empty,(0,0)) $ concat t2

adjacent :: Pos -> [Pos]
adjacent (x,y) = [(x+a,y+b) | b <- [-1,0,1], a <- [-1,0,1]]

getNumber :: Pos -> Image -> Int
getNumber (x,y) (light,img) = intFromBoolList l 0 where
    intFromBoolList [] acc = acc
    intFromBoolList (x:xs) acc = intFromBoolList xs (2*acc + if x == light then 1 else 0)
    l = map (`S.member` img) $ adjacent (x,y)

applyStep :: Image -> IEA -> Image
applyStep (light,img) (flip,iea) = (not (light && flip),modifiedPoints) where
    !pointsToLookAt = foldl' (\s p -> S.union s $ S.fromList $ adjacent p) S.empty img
    modifiedPoints = foldl' (\s p -> if iea ! getNumber p (light,img) == light `xor` flip then p `S.insert` s else s) S.empty pointsToLookAt
    b1 `xor` b2 = if b1 then not b2 else b2