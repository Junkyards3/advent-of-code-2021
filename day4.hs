import System.IO
import Data.List.Split
import Data.List
import Debug.Trace 

main :: IO ()
main = do
   inputFile <- openFile "input/input_day4.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   putStrLn "Results : " ;
   print (parts fileContent) ;

processBingo :: String -> [[(Integer, Bool)]]
processBingo s = map (map (\ nb -> (read nb, False)) .
    filter (not . null) . splitOn " ") (lines s)

check :: Integer -> (Integer, Bool) -> (Integer, Bool)
check draw (i,b) 
    | draw == i = (i, True)
    | otherwise = (i, b)

isWinning :: [[(Integer, Bool)]] -> Bool 
isWinning t = any (all snd) t || any (all snd) (transpose t)

computeResult :: Integer -> [Integer] -> Integer
computeResult draw l = draw * sum l

selectWinning :: Integer -> [[[(Integer, Bool)]]]  -> Maybe Integer
selectWinning _ [] = Nothing
selectWinning draw (x : xs)
    | isWinning x = 
        Just (computeResult draw (map fst (filter (not . snd) (concat x)) ) )
    | otherwise = selectWinning draw xs 

parts :: String -> (Maybe Integer, Maybe Integer)
parts l = (part1Aux listDraws listBoards , part2Aux listDraws listBoards)  where 

    pr :: [String]
    pr = splitOn "\n\n" l

    listDraws :: [Integer]
    listDraws = map read (splitOn "," (head pr))

    listBoards :: [[[(Integer, Bool)]]]
    listBoards = map processBingo (tail pr)

    part1Aux :: [Integer] -> [[[(Integer, Bool)]]] -> Maybe Integer
    part1Aux [] lb = Nothing
    part1Aux (draw : xs) lb = 
            case selectWinning draw checked of 
                Nothing -> part1Aux xs checked
                a -> a                          
            where checked = map (map (map (check draw))) lb

    part2Aux :: [Integer] -> [[[(Integer, Bool)]]] -> Maybe Integer
    part2Aux _ [] = Nothing 
    part2Aux (draw : xs) [x] =
            case selectWinning draw [checked] of 
                Nothing -> part2Aux xs [checked]
                a -> a
                where 
            checked = map (map (check draw)) x
    part2Aux (draw : xs) lb = part2Aux xs (filter (not . isWinning) checked) where
            checked = map (map (map (check draw))) lb

    

    

     
