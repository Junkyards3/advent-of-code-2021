{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Char (toLower)

type Node = String
type Graph = Map Node [Node]
type Path = (Node,Set.Set Node,Bool)
{- first component is the next node in the path,
second is the set of small caves already visited in the path,
third is a bool remembering if a small cave was visited twice -}

main :: IO ()
main = do
   inputFile <- openFile "input/input_day12.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   let g = process $ lines fileContent ;
   print (extendPaths extendPath g) ;
   putStrLn "Result for part 2 : " ;
   print (extendPaths extendPath2 g) ;

process :: [String] -> Graph
process = foldr ((\[s1,s2] g -> Map.insertWith (++) s2 [s1]
          $ Map.insertWith (++) s1 [s2] g) . splitOn "-")
          Map.empty

isLower :: String -> Bool
isLower s = map toLower s == s

extendPath :: Graph -> Path -> [Path]
extendPath g (n,s,b)
    | n == "start" = []
    | otherwise = map (\h -> (h,if isLower h then h `Set.insert` s
           else s,b))
            $ filter (`Set.notMember` s) $ g Map.! n

extendPath2 :: Graph -> Path -> [Path]
extendPath2 g (n,s,isDouble)
    | n == "start" = []
    | isDouble =  map (\h -> (h,if isLower h then h `Set.insert` s
           else s,True))
            $ filter (`Set.notMember` s) $ g Map.! n
    | otherwise = map (\h -> (h,if isLower h then h `Set.insert` s
           else s, h `Set.member` s))
            $ filter (\h -> (h /= "start" && h /= "end") || 
            h `Set.notMember` s )
            $ g Map.! n

extendPaths :: (Graph -> Path -> [Path]) -> Graph -> Int
extendPaths f g = extendPaths' [("end",Set.singleton "end",False)] 0 where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    extendPaths' :: [Path] -> Int -> Int
    extendPaths' buf acc
        | null buf = acc
        | otherwise = extendPaths' (concatMap (f g) buf)
            (acc + length (filter (\x -> fst3 x == "start") buf))