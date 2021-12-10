import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List ( elemIndex, sort )
import Data.Maybe ( fromJust )

main :: IO ()
main = do
   inputFile <- openFile "input/input_day10.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Result for part 1 : " ;
   print (sum $ map (compute . fst . process) (lines fileContent)) ;
   putStrLn "Result for part 2 : " ;
   print (median $ filter (/= 0) $ 
    map (compute2 . snd . process) (lines fileContent)) ;

listPairs :: [(Char,Char)]
listPairs = [('(',')'),('[',']'),('{','}'),('<','>')]

process :: String -> (Maybe Char,String)
process s = processAux s [] where
    processAux :: String -> String -> (Maybe Char,String)
    processAux [] ys = (Nothing,ys)
    processAux (x:xs) [] = processAux xs [x]
    processAux (x:xs) (y:ys)
        | (y,x) `elem` listPairs = processAux xs ys
        | x `elem` map snd listPairs = (Just x,"")
        | otherwise = processAux xs (x:y:ys)

compute :: Maybe Char -> Integer
compute (Just ')') = 3
compute (Just ']') = 57
compute (Just '}') = 1197
compute (Just '>') = 25137
compute _ = 0

compute2 :: String -> Int
compute2 s = compute2Aux s 0 where 
    compute2Aux :: String -> Int -> Int
    compute2Aux "" i = i 
    compute2Aux (x:xs) i = compute2Aux xs 
        (5*i + 1 + fromJust (elemIndex x (map fst listPairs)))

median :: [Int] -> Int
median l = medianAux (sort l) where 
    medianAux l = l !! (length l `quot` 2)
