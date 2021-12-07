import System.IO ( openFile, hGetContents, IOMode(ReadMode) ) 
import Data.List.Split ( splitOn )
import Data.List ( sort )

main :: IO ()
main = do
   inputFile <- openFile "input/input_day7.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (fuel1 fileContent) ;
   putStrLn "Result for part 2 : " ;
   print (findMin (process fileContent)) ;

process :: String -> [Integer]
process s = sort (map read (splitOn "," s))

median :: [Integer] -> Integer
median l = l !! (length l `quot` 2)

fuel1 :: String -> Integer
fuel1 s = sum (map (\x -> abs (x-a)) l) where
        l = process s
        a = median l

fuel2 :: [Integer] -> Integer -> Integer
fuel2 l a = sum (map (\x -> abs (x-a)*(abs (x-a) + 1) `quot` 2) l)

findMin :: [Integer] -> Integer
findMin l = foldr (min . fuel2 l) (fuel2 l 0) [(minimum l)..(maximum l)]