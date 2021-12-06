{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split ( splitOn )

main :: IO ()
main = do
   inputFile <- openFile "input/input_day5.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (part getVentLine fileContent) ;
   putStrLn "Result for part 2 : " ;
   print (part getVentLine2 fileContent) ;

type Coordinates = (Integer,Integer)
type Diagram = Map Coordinates Integer

processLine :: String -> (Coordinates,Coordinates)
processLine s = case map (splitOn ",") (splitOn " -> " s) of 
     [[t1,t2],[t3,t4]] -> ((read t1, read t2),(read t3, read t4))

getVentLine :: (Coordinates,Coordinates) -> [Coordinates]
getVentLine ((x1,y1),(x2,y2)) 
   | x1 == x2  = [(x1,j) | j <- [(min y1 y2)..(max y1 y2)]]
   | y1 == y2  = [(i,y1) | i <- [(min x1 x2)..(max x1 x2)]]
   | otherwise = []

getVentLine2 :: (Coordinates,Coordinates) -> [Coordinates]
getVentLine2 ((x1,y1),(x2,y2)) 
   | x1 == x2  = [(x1,j) | j <- [(min y1 y2)..(max y1 y2)]]
   | y1 == y2  = [(i,y1) | i <- [(min x1 x2)..(max x1 x2)]]
   | x2 - x1 == y2 - y1 = [(i,y1-x1+i) | i <- [(min x1 x2)..(max x1 x2)]]
   | x2 - x1 == y1 - y2 = [(i,y1+x1-i) | i <- [(min x1 x2)..(max x1 x2)]]
   | otherwise = []
      
cover :: ((Coordinates,Coordinates) -> [Coordinates]) -> 
            [(Coordinates,Coordinates)] -> Diagram 
cover f t = coverAux f t Map.empty where

    coverAux :: ((Coordinates,Coordinates) -> [Coordinates]) -> 
       [(Coordinates,Coordinates)] -> Diagram -> Diagram
    coverAux f xs diag = foldl
      (\ d2 c2 -> foldl (\ d c -> Map.insertWith (+) c 1 d) d2 
         (f c2))
      diag xs

part :: ((Coordinates,Coordinates) -> [Coordinates]) -> String -> Int
part f s = Map.size (Map.filter (>= 2) (cover f (map processLine (lines s))))