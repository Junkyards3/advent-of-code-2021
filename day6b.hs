import Data.Matrix
    ( fromList, identity, matrix, multStd, toList, Matrix(nrows), zero, setElem, (!) )
import Data.Time ()
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.List.Split ( splitOn )


main :: IO ()
main = do
   inputFile <- openFile "input/input_day6.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (process fileContent) ;
   print (res fileContent 80) ;
   putStrLn "Result for part 2 : " ;
   print (res fileContent 1000000) ;

mat :: Matrix Integer
mat = matrix 9 9 h where
    h :: (Int,Int) -> Integer
    h (i,j)
        | i + 1 == j || i == 9 && j == 1 || i == 7 && j == 1 = 1
        | otherwise = 0

process :: String -> Matrix Integer
process s = foldr ((\ x m -> setElem (1 + (m ! (x+1,1))) (x+1,1) m) . read) (zero 9 1) (splitOn "," s)

matPower :: Matrix Integer -> Integer -> Matrix Integer
matPower m 0 = identity (nrows m)
matPower m n
    | even n = multStd mp mp
    | otherwise = multStd m (multStd mp mp)
    where mp = matPower m (n `quot` 2)

res :: String -> Integer -> Integer
res s n = sum . toList $ multStd (matPower mat n) (process s)