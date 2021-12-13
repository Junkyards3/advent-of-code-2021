import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List ( sortBy )
import Data.Array ( Array, (!), bounds, indices, listArray )
import Data.Char (digitToInt)
import qualified Data.Set as Set

main :: IO ()
main = do
   inputFile <- openFile "input/input_day9.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   let tab = process fileContent ;
   print (riskMinimals tab) ;
   putStrLn "Result for part 2 : " ;
   print (part2 tab) ;

process :: String -> Array (Int,Int) Int
process s = listArray ((1,1),(m,n)) (map digitToInt $ filter (/= '\n') s) where
    l = lines s
    m = length l
    n = length (head l)

adjacent :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [(Int,Int)]
adjacent ((m1,n1),(m,n)) (x,y) = filter (\(a,b) -> a >= 1 && b >= 1 && a <= m && b <= n)
    [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

isMinimal :: (Int,Int) -> Array (Int,Int) Int -> Bool
isMinimal (a,b) t = all (((t ! (a,b)) <) . (t !)) (adjacent (bounds t) (a,b))

getMinimals :: Array (Int,Int) Int -> [(Int,Int)]
getMinimals t = filter (`isMinimal` t) (indices t)

riskMinimals :: Array (Int,Int) Int -> Int
riskMinimals t = sum $ map (\x -> 1 + t ! x) $ getMinimals t

findBasin :: Array (Int,Int) Int -> (Int,Int) -> Set.Set (Int,Int)
findBasin t (a,b) = case l of 
    [] -> Set.singleton (a,b)
    _ -> Set.insert (a,b) $ foldl Set.union Set.empty (map (findBasin t) l)
    where l = filter (\x -> t ! x > t ! (a,b) && t ! x /= 9) (adjacent (bounds t) (a,b))

part2 :: Array (Int,Int) Int -> Int 
part2 t = product $ take 3 $ sortBy (flip compare) $ map (length . findBasin t) (getMinimals t)