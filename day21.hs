import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State (evalState, get, modify)
import Data.Bifunctor (Bifunctor(bimap))

main :: IO ()
main = do
    inputFile <- openFile "input/input_day20.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 2 : " ;
    let (w1,w2) = memoGetNbWins (GameState True 0 0 2 8) ;
    print $ max w1 w2 ;

space :: Integer -> Integer -> Bool -> Integer
space sPos n isPlayer1 = let step = if isPlayer1 then 6 else 5 in
    (sPos + step*n - n*(n-1) - 1) `mod` 10 + 1

score :: Integer -> Integer -> Bool -> Integer
score sPos n isPlayer1 = sum [space sPos m isPlayer1 | m <- [1..n]]

data GameState = GameState Bool Integer Integer Integer Integer deriving (Eq,Ord,Show)
--next player, score p1, score p2, pos p1, pos p2

spMod10 :: Integral a => a -> a
spMod10 s = (s-1) `mod` 10 + 1

getNbWins :: Monad m => (GameState -> m (Integer,Integer)) -> GameState -> m (Integer,Integer)
getNbWins recNbWins (GameState nextIs1 s1 s2 p1 p2)
    | s1 >= 21 = return (1,0)
    | s2 >= 21 = return (0,1)
    | nextIs1 =   (let fg = (sum . zipWith (*) [1,3,6,7,6,3,1]) in (bimap fg fg . unzip)) <$> mapM recNbWins
        [GameState False (s1 + spMod10 (p1 + dice)) s2 (spMod10 (p1 + dice)) p2 | dice <- [3..9]]
    | otherwise = (let fg = (sum . zipWith (*) [1,3,6,7,6,3,1]) in (bimap fg fg . unzip)) <$> mapM recNbWins
        [GameState True  s1 (s2 + spMod10 (p2 + dice)) p1 (spMod10 (p2 + dice)) | dice <- [3..9]]

memoGetNbWins :: GameState -> (Integer,Integer)
memoGetNbWins gs = evalState (getNbWins recNbWins gs) M.empty
    where recNbWins gs' = do
            v <- M.lookup gs' <$> get
            case v of
              Just p -> return p
              Nothing -> do
                  p <- getNbWins recNbWins gs'
                  modify $ M.insert gs' p
                  return p