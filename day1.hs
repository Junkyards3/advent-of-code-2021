import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

main :: IO ()
main = do
   inputFile <- openFile "input/input_day1.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   let fileLines = lines fileContent ;
   putStrLn "Result for part 1 : " ;
   print (f (map read fileLines)) ;
   putStrLn "Result for part 2 : " ;
   print (f (slide (map read fileLines)))

f :: [Integer] -> Integer
f [] = 0
f [x] = 0
f (x : (y : xs)) 
    | x < y = 1 + f (y : xs)
    | otherwise = f (y : xs)

slide :: [Integer] -> [Integer]
slide (x : y : z : xs) = (x + y + z) : slide (y : z : xs)
slide _ = []