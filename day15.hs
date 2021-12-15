import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Debug.Trace
import Data.Array.ST
import Data.Array

type Coordinates = (Int,Int)
type Cavern = Array Coordinates Int
data InfInt = Fin Int | Infinity deriving (Eq,Show)

instance Ord InfInt where
    Fin a `compare` Fin b = a `compare` b
    Infinity `compare` Fin b = GT
    Fin a `compare` Infinity = LT
    _ `compare` _ = EQ

add :: InfInt -> InfInt -> InfInt
add (Fin a) (Fin b) = Fin $ a + b
add _ _ = Infinity

main :: IO ()
main = do
    inputFile <- openFile "input/input_day15.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    let (t,n) = process fileContent;
    putStrLn "Result for part 1 : " ;
    print $ distance t n (1,1) (n,n);
    putStrLn "Result for part 2 : " ;

process :: String -> (Cavern,Int)
process s = (array ((1,1),(n,n)) [(\ (a, b) -> ((a + 1, b + 1), digitToInt $ (ls !! a) !! b))
   (i, j) |
   i <- [0 .. length ls - 1], j <- [0 .. length ls - 1]],length ls)
        where ls = lines s
              n = length ls

adjacent :: Coordinates -> Int -> [Coordinates]
adjacent (a,b) n = filter (\(x,y) -> x >= 1 && y >= 1 && x <= n && y <= n)
    [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]

distance :: Cavern -> Int -> Coordinates -> Coordinates -> InfInt
distance cav n (ae,be) (as,bs) =  dij (Map.fromList ([ ((i, j), if (i,j) == (ae,be) then Fin 0 else Infinity) | 
    i <- [1 .. n], j <- [1..n]])) Map.! (as,bs) where

    maj :: STUArray Coordinates InfInt -> Coordinates -> Map.Map Coordinates InfInt
    maj m c = foldr (\c' m' -> Map.insert c' (min (m Map.! c') (m Map.! c `add` Fin (cav ! c'))) m') m (adjacent c n)

    dij m = case dij' (m,Map.keysSet m) of
            (m',_) -> m'

    dij' :: (Map.Map Coordinates InfInt, Set.Set Coordinates) -> (Map.Map Coordinates InfInt, Set.Set Coordinates)
    dij' (m,s)
        | sMin == (as,bs) = (m,s)
        | trace (show sMin) otherwise = dij' (maj m sMin, Set.delete sMin s) where
            sMin = Set.foldr (\newS sMin -> if m Map.! newS < m Map.! sMin then newS else sMin) (Set.elemAt 0 s) s