import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Maybe (isJust)
import Data.Char (isDigit)
import qualified Data.Bifunctor

type Depth = Int
data SnailNum = Regular Int Depth | Pair SnailNum SnailNum Depth deriving (Eq)

instance Show SnailNum where
    show (Regular i d) = show i
    show (Pair sn1 sn2 d) = "[" ++ show sn1 ++ "," ++ show sn2 ++ "]"

addSn :: SnailNum -> SnailNum -> SnailNum
sn1 `addSn` sn2 = reduce $ Pair (addDepth sn1) (addDepth sn2) 0

main :: IO ()
main = do
    inputFile <- openFile "input/input_day18.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;
    let t = map parse $ lines fileContent ;
    print $ magnitude $ foldl1 addSn t ;
    putStrLn "Result for part 2 : " ;
    print $ maximum [magnitude $ sn1 `addSn` sn2 | sn1 <- t, sn2 <- t, sn1 /= sn2]

addDepth :: SnailNum -> SnailNum
addDepth (Regular i d) = Regular i (d+1)
addDepth (Pair sn1 sn2 d) = Pair (addDepth sn1) (addDepth sn2) (d+1)

parse :: String -> SnailNum
parse = parseAux 0 where
    splitOuterComma :: Int -> String -> (String,String)
    splitOuterComma _ "" = ("","")
    splitOuterComma i (x:xs)
        | x == ',' && i == 0 = ("",xs)
        | x == '[' = Data.Bifunctor.first (x :) $ splitOuterComma (i+1) xs
        | x == ']' = Data.Bifunctor.first (x :) $ splitOuterComma (i-1) xs
        | otherwise = Data.Bifunctor.first (x :) $ splitOuterComma i xs

    parseAux :: Int -> String -> SnailNum
    parseAux d s
        | all isDigit s = Regular (read s) d
        | otherwise = Pair (parseAux (d+1) s1) (parseAux (d+1) s2) d
            where (s1,s2) = splitOuterComma 0 $ tail $ init s

explode :: SnailNum -> (SnailNum, Maybe Int, Maybe Int, Bool)
explode sn@(Regular _ _) = (sn, Nothing, Nothing, False)
explode sn@(Pair (Regular i1 _) (Regular i2 _) d)
    | d == 4 = (Regular 0 d, Just i1, Just i2, True)
    | otherwise = (sn, Nothing, Nothing, False)
explode (Pair sn1 sn2 d)
    | isJust msg11 || isJust msg12 = (Pair sn1p (propagate sn2 msg12 False) d, msg11, Nothing, True)
    | isExploded1 = (Pair sn1p sn2 d, Nothing, Nothing, True)
    | isJust msg21 || isJust msg22 = (Pair (propagate sn1 msg21 True) sn2p d,  Nothing, msg22, True)
    | otherwise = (Pair sn1 sn2p d, Nothing, Nothing, isExploded2)
        where (sn1p,msg11,msg12,isExploded1) = explode sn1
              (sn2p,msg21,msg22,isExploded2) = explode sn2
              
              propagate :: SnailNum -> Maybe Int -> Bool -> SnailNum
              propagate sn Nothing _ = sn
              propagate (Regular i d) (Just im) _ = Regular (i+im) d
              propagate (Pair sn1 sn2 d) msg True  = Pair sn1 (propagate sn2 msg True) d
              propagate (Pair sn1 sn2 d) msg False = Pair (propagate sn1 msg False) sn2 d

split :: SnailNum -> (SnailNum, Bool)
split sn@(Regular i d)
    | i >= 10 = (Pair (Regular q (d+1)) (Regular (q+r) (d+1)) d,True)
    | otherwise = (sn,False)
        where (q,r) = quotRem i 2
split (Pair sn1 sn2 d)
    | isSplit1  = (Pair sn1p sn2 d,True)
    | otherwise = (Pair sn1 sn2p d, isSplit2)
        where (sn1p,isSplit1) = split sn1
              (sn2p,isSplit2) = split sn2

reduce :: SnailNum -> SnailNum
reduce sn
    | isExploded = reduce snp
    | isReduced  = reduce snp1
    | otherwise  = snp1
        where (snp,_,_,isExploded) = explode sn
              (snp1,isReduced)     = split sn

magnitude :: SnailNum -> Int
magnitude (Regular i _) = i
magnitude (Pair sn1 sn2 _) = 3 * magnitude sn1 + 2 * magnitude sn2