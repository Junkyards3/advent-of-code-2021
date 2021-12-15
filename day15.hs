import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.Array ( Array, (!), array, assocs, bounds )
import Data.Maybe (fromJust)

type Coordinates = (Int,Int)
type Cavern = Array Coordinates Int
data InfInt = Fin Int | Infinity deriving (Eq,Show)

instance Ord InfInt where
    Fin a `compare` Fin b = a `compare` b
    Infinity `compare` Fin b = GT
    Fin a `compare` Infinity = LT
    _ `compare` _ = EQ

instance Semigroup InfInt where
  Fin a <> Fin b = Fin $ a + b 
  _ <> _ = Infinity

main :: IO ()
main = do
    inputFile <- openFile "input/input_day15.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    let (t,n) = process fileContent;
    putStrLn "Result for part 1 : " ;
    print $ distance t n (1,1) (n,n);
    putStrLn "Result for part 2 : " ;
    print $ distance (increase t) (5*n) (1,1) (5*n,5*n);

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
distance cav lg (ae,be) (as,bs) = dij (Map.fromList [ (if (i,j) == (ae,be) then Fin 0 else Infinity,Set.singleton (i,j)) |
    i <- [1..lg], j <- [1..lg]]) Set.empty where

    dij :: Map.Map InfInt (Set.Set Coordinates) -> Set.Set Coordinates -> InfInt
    dij valuePath visited
        | (as,bs) `Set.member` sMin = vMin
        | otherwise = dij newValuePath newVisited  where
            ((vMin,sMin), valuePathMinRemoved) = fromJust $ Map.minViewWithKey valuePath
            expanded = Set.difference (Set.fromList $ concatMap (`adjacent` lg) sMin) visited
            newVisited = Set.union visited sMin
            newValuePath = foldl (\m (a,b) -> Map.insertWith Set.union (vMin <> Fin (cav ! (a,b))) (Set.singleton (a,b)) m) 
                        valuePathMinRemoved expanded

increase :: Cavern -> Cavern 
increase t = array ((1,1),(5*b3,5*b4)) [((x+b3*i,y+b4*j),(ii + (i+j) - 1) `mod` 9 + 1) | 
    i <- [0..4], j <- [0..4], ((x,y),ii) <- assocs t] where ((b1,b2),(b3,b4)) = bounds t