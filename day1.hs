import System.IO

main :: IO ()
main = do
   helloFile <- openFile "input_day1.txt" ReadMode ;
   fileContent <- hGetContents helloFile ;
   let fileLines = lines fileContent ;
   print (f (map read fileLines)) ;
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