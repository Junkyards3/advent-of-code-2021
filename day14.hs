import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Rules = Map.Map (Char,Char) Char
type Polymer = String

main :: IO ()
main = do
    inputFile <- openFile "input/input_day14.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    let t = process fileContent ;
    let chars = foldr Set.insert Set.empty (Map.elems $ snd t)
    putStrLn "Result for part 1 : " ;
    let m1 = Set.map (f t 10) chars ;
    print $ Set.findMax m1 - Set.findMin m1 ;
    putStrLn "Result for part 2 : " ;
    let m2 = Set.map (f t 40) chars 
    print $ Set.findMax m2 - Set.findMin m2 ;

process :: String -> (Polymer,Rules)
process s = (c1,c2) where
    t = splitOn "\n\n" s
    c1 = head t
    c2 = foldr ((\l s -> Map.insert (head $ head l,head l !! 1) (head $ l !! 1) s) . splitOn " -> ") Map.empty (lines (t !! 1))

transf :: [a] -> [[a]]
transf [] = []
transf [x] = []
transf (x1:x2:c) = [x1,x2] : transf (x2:c)

f :: (Polymer,Rules) -> Int -> Char -> Integer
f (p,lr) n c = snd $ f' p lr n c Map.empty where
    f' :: Polymer -> Rules -> Int -> Char -> Map.Map (Polymer,Int,Char) Integer -> (Map.Map (Polymer,Int,Char) Integer,Integer)
    f' p lr n c m
        | n == 0 = (Map.insert (p,0,c) x''' m,x''')
        | (p,n,c) `Map.member` m = (m,m Map.! (p,n,c))
        | length p == 2 = (Map.insert (p,n,c) x' m',x')
        | otherwise = (Map.insert (p,n,c) x'' m'',x'')
            where (m',x') = f' [head p,lr Map.! (head p, p !! 1),p !! 1] lr (n-1) c m
                  (m'',x'') = foldr (\pp (ma,xa) -> case f' pp lr n c ma of (mb,xb) -> (mb,xa+xb)) (m,negate $ 
                    toInteger $ length $ filter (== c) $ tail $ init p) (transf p)
                  x''' = toInteger $ length $ filter (== c) p