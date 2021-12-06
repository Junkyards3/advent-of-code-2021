import System.IO
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)

main :: IO ()
main = do
   inputFile <- openFile "input/input_day6.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (nbLanterns (iterate update (process fileContent) !! 80)) ;
   putStrLn "Result for part 2 : " ;
   print (nbLanterns (iterate update (process fileContent) !! 256)) ;

type Lanterns = Map Integer Integer

process :: String -> Lanterns
process s = foldr (\ x -> Map.insertWith (+) (read x) 1) Map.empty (splitOn "," s)

update :: Lanterns -> Lanterns
update lt = Map.insertWith (+) 6 (Map.findWithDefault 0 0 lt) (Map.insert 8 (Map.findWithDefault 0 0 lt) 
   (foldl (\d x -> Map.insert x (Map.findWithDefault 0 (x+1) lt) d) Map.empty [0..7]))

nbLanterns :: Lanterns -> Integer
nbLanterns = Map.foldr (+) 0