
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Debug.Trace 

type Paper = Set.Set (Int,Int)
type Inst = (Bool,Int)

main :: IO ()
main = do
    inputFile <- openFile "input/input_day13.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;
    let pi = process fileContent ;
    print $ Set.size . fst $ foldPaper pi;
    putStrLn "Result for part 2 : " ;
    putStrLn $ affich $ foldUntil pi; 


process :: String -> (Paper,[Inst])
process s = (c1,c2) where
    t = splitOn "\n\n" s
    p = map (splitOn ",") $ splitOn "\n" (head t)
    i = map (\a -> splitOn "=" $ splitOn " " a !! 2) $ splitOn "\n" (t !! 1)
    c1 = foldr (\u s -> (read $ head u, read $ u !! 1) `Set.insert` s) Set.empty p
    c2 = map (\a -> (head a == "x",read $ a !! 1)) i

foldPaper :: (Paper,[Inst]) -> (Paper,[Inst])
foldPaper (p,[]) = (p,[])
foldPaper (p,(isVert,pos):is)
    | isVert =(Set.foldr (\(x,y) s -> (pos - abs (pos - x) ,y) `Set.insert` s) Set.empty p,is)
    | otherwise = (Set.foldr (\(x,y) s -> (x,pos - abs (pos - y)) `Set.insert` s) Set.empty p,is)

foldUntil :: (Paper,[Inst]) -> Paper
foldUntil (p,[]) = p
foldUntil pi = foldUntil $ foldPaper pi

affich :: Paper -> String
affich p = affichRec 0 p where
    affichRec :: Int -> Set.Set (Int,Int) -> String
    affichRec v p 
        | v > Set.findMax (Set.map snd p) = ""
        | otherwise = affichRec2 0 (Set.toAscList (Set.filter (\(a,b) -> b == v) p)) ++ '\n':affichRec (v+1) p
    
    affichRec2 :: Int -> [(Int,Int)] -> String 
    affichRec2 h [] = ""
    affichRec2 h ((a,b):xs)
        | h == a = '#':affichRec2 (h+1) xs 
        | otherwise = '.':affichRec2 (h+1) ((a,b):xs)