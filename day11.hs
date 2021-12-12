import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = do
   inputFile <- openFile "input/input_day11.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   let t = process fileContent ;
   print (nbFlashes t 100);
   putStrLn "Result for part 2 : " ;
   print (whenSynchro t) ;

type OctopusGrid = Map.Map (Int,Int) (Int,Bool)

process :: String -> OctopusGrid
process s = foldr (\(x,y) o ->  Map.insert (x,y) ((t !! (x-1)) !! (y-1),False) o) Map.empty
    [(i,j) | i <- [1..10], j <- [1..10]] 
    where t = map (map digitToInt) $ lines s

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (a,b) = filter (\(x,y) -> x >= 1 && y >= 1 && x <= 10 && y <= 10 && (x /= a || y /= b))
    [(i,j) | i <- [a-1,a,a+1], j <- [b-1,b,b+1]]

step :: OctopusGrid -> (OctopusGrid, Int)
step = step3 . step2 0 . step1 where
    step1 :: OctopusGrid -> OctopusGrid
    step1 = Map.map (\(i,b) -> (i+1,b))

    step2 :: Int -> OctopusGrid -> (OctopusGrid, Int)
    step2 nbFlash og = case flashed of
        [] -> (og,nbFlash)
        fl -> step2 (length fl + nbFlash) $ foldr (\p o ->
            foldr (Map.adjust(\(i,b) -> (i+1,b))) (Map.insert p (0,True) o) (adjacent p))
            og fl
        where flashed = Map.keys $ Map.filter (\(i,b) -> not b && i >= 10) og

    step3 :: (OctopusGrid, Int) -> (OctopusGrid, Int)
    step3 (og,v) = (Map.map (\(i,b) -> if b then (0,False) else (i,b)) og,v)

nbFlashes :: OctopusGrid -> Int -> Int
nbFlashes og 0 = 0
nbFlashes og n = m + nbFlashes og' (n-1) where
    (og',m) = step og

whenSynchro :: OctopusGrid -> Int
whenSynchro og
    | null $ Map.filter (/= (0,False)) og = 0
    | otherwise = 1 + (whenSynchro . fst . step) og 