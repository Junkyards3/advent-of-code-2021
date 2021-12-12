import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Char (toLower)
import Debug.Trace 

type Node = String
type Graph = Map Node [Node]
type Path = ([Node],Set.Set Node,Bool)
main :: IO ()
main = do
   inputFile <- openFile "input/input_day12.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   let g = process $ lines fileContent ;
   print (length $ extendPaths extendPath g) ;
   putStrLn "Result for part 2 : " ;
   print (length $ extendPaths extendPath2 g) ;

process :: [String] -> Graph
process = foldr ((\(s1,s2) g -> Map.insertWith (++) s2 [s1]
          $ Map.insertWith (++) s1 [s2] g) . tuplize .splitOn "-")
          Map.empty
    where tuplize :: [String] -> (String,String)
          tuplize [a,b] = (a,b)
          tuplize _ = ("","")

isLower :: String -> Bool
isLower s = map toLower s == s

extendPath :: Graph -> Path -> [Path]
extendPath _ ([],_,_) = []
extendPath g (x:xs,s,b)
    | x == "start" = []
    | otherwise = foldr (\h -> ((h:x:xs,if isLower h then h `Set.insert` s
            else s,b):)) []
            $ filter (`Set.notMember` s) $ g Map.! x

extendPath2 :: Graph -> Path -> [Path]
extendPath2 _ ([],_,_) = []
extendPath2 g (x:xs,s,isDouble)
    | x == "start" = []
    | isDouble =  foldr (\h -> ((h:x:xs,if isLower h then h `Set.insert` s
            else s,True):)) []
            $ filter (`Set.notMember` s) $ g Map.! x
    | otherwise = foldr (\h -> ((h:x:xs,if isLower h then h `Set.insert` s
            else s, h `Set.member`s):))
            [] $ filter (\h -> h `Set.notMember` s ||
            (h /= "start" && h /= "end"))
            $ g Map.! x

extendPaths :: (Graph -> Path -> [Path]) -> Graph -> [Path]
extendPaths f g = extendPaths' [(["end"],Set.singleton "end",False)] [] where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    extendPaths' :: [Path] -> [Path] -> [Path]
    extendPaths' buf acc
        | null buf = acc
        | otherwise = extendPaths' (concatMap (f g) buf)
            (acc ++ filter (\x -> (head . fst3) x == "start") buf)