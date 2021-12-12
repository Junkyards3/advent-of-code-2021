import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (toLower)
import Debug.Trace


type Node = String
type Graph = Map Node [Node]

main :: IO ()
main = do
   inputFile <- openFile "input/input_day12.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   let g = process $ lines fileContent ;
   print (Set.size $ extendPaths g) ;
   putStrLn "Result for part 2 : " ;
   print (Set.size $ extendPaths2 g) ;

process :: [String] -> Graph
process = foldr ((\(s1,s2) g -> Map.insertWith (++) s2 [s1]
          $ Map.insertWith (++) s1 [s2] g) . tuplize .splitOn "-")
          Map.empty
    where tuplize :: [String] -> (String,String)
          tuplize [a,b] = (a,b)
          tuplize _ = ("","")

extendPath :: Graph -> [Node] -> Set.Set [Node]
extendPath _ [] = Set.singleton []
extendPath g (x:xs) = foldr (\h -> Set.insert (h:x:xs))
            Set.empty $ filter (\a -> a `notElem` xs || map toLower a /= a)
            $ g Map.! x

extendPath2 :: Graph -> [Node] -> Set.Set [Node]
extendPath2 _ [] = Set.singleton []
extendPath2 g (x:xs) = foldr (\h -> Set.insert (h:x:xs))
            Set.empty $ filter (\a -> map toLower a /= a ||
            (a /= "start" && a /= "end" && (notElem a xs
            || not (isDoubleSmallCave (x:xs)) )) ||
            a `notElem` xs)
            $ g Map.! x

            where
                isDoubleSmallCave [] = False
                isDoubleSmallCave (x:xs) = (map toLower x == x && x `elem` xs)
                    || isDoubleSmallCave xs


extendPaths :: Graph -> Set.Set [Node]
extendPaths g = extendPaths' g (Set.singleton ["end"]) Set.empty where
    extendPaths' :: Graph -> Set.Set [Node] -> Set.Set [Node] -> Set.Set [Node]
    extendPaths' g buf acc
        | buf `Set.isSubsetOf` acc = Set.filter (\x -> head x == "start") acc
        | otherwise = extendPaths' g (Set.unions $ Set.map (extendPath g) buf)
            (acc `Set.union` buf)

extendPaths2 :: Graph -> Set.Set [Node]
extendPaths2 g = extendPaths2' g (Set.singleton ["end"]) Set.empty where
    extendPaths2' :: Graph -> Set.Set [Node] -> Set.Set [Node] -> Set.Set [Node]
    extendPaths2' g buf acc
        | buf `Set.isSubsetOf` acc = Set.filter (\x -> head x == "start") acc
        | otherwise = extendPaths2' g (Set.unions $ Set.map (extendPath2 g) buf)
            (acc `Set.union` buf)
