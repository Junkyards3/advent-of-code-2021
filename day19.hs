{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (sort, intersect, sortBy)
import Data.Matrix (toList, Matrix, fromList, fromLists, multStd, inverse, zero, mapPos, toLists)
import Data.Ratio (numerator)
import Data.Maybe (isJust)

main :: IO ()
main = do
    inputFile <- openFile "input/input_day19.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for parts 1 and 2 : " ;
    let t = process fileContent ;
    let t1 = head t ;
    let t2 = t !! 1;
    print $ fuseFinalLength t ;

type Beacon = (Integer,Integer,Integer)
type Scanner = (Integer,Integer,Integer)
type SBMap = S.Set Beacon
type SBMapD = M.Map Beacon (M.Map Integer Int)

tuplize :: [c] -> (c, c, c)
tuplize [x,y,z] = (x,y,z)

untuplize :: (a, a, a) -> [a]
untuplize (x,y,z) = [x,y,z]

process :: String -> [SBMap]
process s = map (S.fromList . map (tuplize . map read . splitOn ",") . tail . splitOn "\n") (splitOn "\n\n" s)

dist1 :: Beacon -> Beacon -> Integer
dist1 (x1,y1,z1) (x2,y2,z2) = abs(x1-x2) + abs(y1-y2) + abs(z1-z2)

toDistMap :: SBMap -> SBMapD
toDistMap sm = foldr (\(b1,b2) m' -> M.insertWith (M.unionWith (+)) b1 (M.singleton (dist1 b1 b2) 1) m') M.empty
    [(b1,b2)| b1 <- S.toList sm, b2 <- S.toList sm, b1 /= b2]

mostAlikePoints :: SBMapD -> SBMapD -> [(Beacon,Beacon,Int)]
mostAlikePoints sb1 sb2 = sortBy (\(_,_,s1) (_,_,s2) -> s2 `compare` s1)
    [(b1,b2,M.foldr (+) 0 $ M.intersectionWith min (sb1 M.! b1) (sb2 M.! b2)) | b1 <- M.keys sb1, b2 <- M.keys sb2]

getTransfo :: [(Beacon,Beacon)] -> Matrix Integer
getTransfo l = case amm of
    Left s -> zero 2 2
    Right am -> mapPos (\ _ v -> numerator v) $ multStd am r
    where r = fromLists $ take 4 $ map (map fromIntegral . untuplize . snd) l
          a = fromLists $ take 4 $ map (map fromIntegral . (\u -> u ++ [1]) . untuplize . fst) l
          amm = inverse a

applyTransfo :: Beacon -> Matrix Integer -> Beacon
applyTransfo x m = tuplize . toList $ multStd (fromList 1 4 $ (++ [1]) $ untuplize x) m

fuseTwo :: (SBMapD,[Scanner]) -> (SBMapD,[Scanner]) -> Maybe (SBMapD,[Scanner])
fuseTwo (sb2,ls2) (sb1,ls1)
    | all ((>= 11) . (\(_,_,c) -> c)) (take 11 mAP) = Just (M.unionWith (M.unionWith max) sb2 $ M.mapKeys (`applyTransfo` m) sb1,
        ls2 ++ map (`applyTransfo` m) ls1)
    | otherwise = Nothing
    where mAP = mostAlikePoints sb1 sb2
          m = getTransfo $ map (\(a,b,_) -> (a,b)) mAP

fuseFinal :: [(SBMapD,[Scanner])] -> Maybe (SBMapD,[Scanner])
fuseFinal [sb] = Just sb
fuseFinal (sb1:rsb) = fuseFinal =<< u
    where u = fuseAux sb1 rsb
          fuseAux :: (SBMapD,[Scanner]) -> [(SBMapD,[Scanner])] -> Maybe [(SBMapD,[Scanner])]
          fuseAux sb1 [] =  Nothing
          fuseAux sb1 (sb2:rsb) = case (tfsb,tfsb2) of
            (Just fsb,_) -> Just $ fsb : rsb
            (_, Just lsb) -> Just $ sb2 : lsb
            (Nothing, Nothing) -> Nothing
            where tfsb = fuseTwo sb1 sb2
                  tfsb2 = fuseAux sb1 rsb

fuseFinalLength :: [SBMap] -> Maybe (Int,Integer)
fuseFinalLength lsb = case u of
    Just (l,ls) -> Just (M.size l,maximum [dist1 sc1 sc2 | sc1 <- ls, sc2 <- ls])
    Nothing -> Nothing
    where u = fuseFinal $ map (\sb -> (toDistMap sb,[(0,0,0)])) lsb