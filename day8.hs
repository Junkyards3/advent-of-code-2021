import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List.Split ( splitOn )
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

main :: IO ()
main = do
   inputFile <- openFile "input/input_day8.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (sum (map (nb1478 .process) (lines fileContent)) );
   putStrLn "Result for part 2 : " ;
   print (sum (map (getValue . process) (lines fileContent)));

process :: String -> ([String],[String])
process s = case splitOn " | " s of
        [a,b] -> (splitOn " " a,splitOn " " b)
        _ -> ([],[])

nb1478 :: ([String],[String]) -> Int
nb1478 (a,b) = length (filter (\x -> length x `elem` [2,3,4,7]) b)

getNumberString :: [String] -> Map Int String
getNumberString a = (put5 . put2 . put9 . put0 . put3 . put6 . put1 . put4 . put7 . put8) Map.empty where

    put1 :: Map Int String -> Map Int String
    put1 = Map.insert 1 (head (filter (\x -> length x == 2) a))

    put4 :: Map Int String -> Map Int String
    put4 = Map.insert 4 (head (filter (\x -> length x == 4) a))

    put7 :: Map Int String -> Map Int String
    put7 = Map.insert 7 (head (filter (\x -> length x == 3) a))

    put8 :: Map Int String -> Map Int String
    put8 = Map.insert 8 (head (filter (\x -> length x == 7) a))

    put6 :: Map Int String -> Map Int String
    put6 m = Map.insert 6 (head (filter (\x -> length x == 6 && length (x `List.intersect` (m Map.! 1)) == 1) a)) m

    put0 :: Map Int String -> Map Int String
    put0 m = Map.insert 0 (head (filter
        (\x -> length x == 6 && length (x `List.intersect` (m Map.! 4)) == 3 && x /= (m Map.! 6)) a)) m

    put9 :: Map Int String -> Map Int String
    put9 m = Map.insert 9 (head (filter
        (\x -> length x == 6 && x /= (m Map.! 0) && x /= (m Map.! 6)) a)) m

    put3 :: Map Int String -> Map Int String
    put3 m = Map.insert 3 (head (filter
        (\x -> length x == 5 && length (x `List.intersect` (m Map.! 1)) == 2) a)) m

    put2 :: Map Int String -> Map Int String
    put2 m = Map.insert 2 (head (filter
        (\x -> length x == 5 && length (x `List.intersect` (m Map.! 4)) == 2) a)) m

    put5 :: Map Int String -> Map Int String
    put5 m = Map.insert 5 (head (filter
        (\x -> length x == 5 && x /= (m Map.! 2) && x /= (m Map.! 3)) a)) m

getValue :: ([String],[String]) -> Int
getValue (a,b) = getNumber (map (findInt m) b) where
    m = getNumberString a

    findInt :: Map Int String -> String -> Int
    findInt m s = (head . Map.keys) m2 where 
        m2 = Map.filter (\a -> List.sort a == List.sort s) m

    getNumber :: [Int] -> Int
    getNumber l = foldl (\i s -> 10 * i + s) 0 l