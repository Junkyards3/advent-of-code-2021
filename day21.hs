import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.List (foldl')

main :: IO ()
main = do
    inputFile <- openFile "input/input_day20.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;

space :: Integer -> Integer -> Bool -> Integer
space sPos n isPlayer1 = let step = if isPlayer1 then 6 else 5 in 
    (sPos + step*n - n*(n-1) - 1) `mod` 10 + 1 

score :: Integer -> Integer -> Bool -> Integer 
score sPos n isPlayer1 = sum [space sPos m isPlayer1 | m <- [1..n]]

data State = State Bool Integer Integer Integer Integer deriving (Eq,Ord,Show)
--next player, score p1, score p2, pos p1, pos p2

spMod10 :: Integral a => a -> a
spMod10 s = (s-1) `mod` 10 + 1

getNbWins :: (M.Map State (Integer,Integer), State) -> (M.Map State (Integer,Integer),(Integer,Integer))
getNbWins (m,s@(State nextIs1 s1 s2 p1 p2))
    | M.member s m = (m,m M.! s)
    | s1 >= 21 = (M.insert s (1,0) m,(1,0))
    | s2 >= 21 = (M.insert s (0,1) m,(0,1))
    | nextIs1 = (\(m',(w1,w2)) -> (M.insert s (w1,w2) m',(w1,w2))) $ 
        foldl' (\(m', (w1,w2)) (dice,nbU) -> let (m'',(a,b)) = getNbWins (m',State False (s1 + spMod10 (p1 + dice)) s2 (spMod10 (p1 + dice)) p2)
        in (m'',(nbU*a + w1,nbU*b + w2))) 
        (m,(0,0)) [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
    | otherwise = (\(m',(w1,w2)) -> (M.insert s (w1,w2) m',(w1,w2))) $
        foldl' (\(m', (w1,w2)) (dice,nbU) -> let (m'',(a,b)) = getNbWins (m',State True s1 (s2 + spMod10 (p2 + dice)) p1 (spMod10 (p2 + dice)))
        in (m'',(nbU*a + w1,nbU*b + w2)) ) 
        (m,(0,0)) [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]