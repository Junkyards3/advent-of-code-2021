import System.IO
import Data.List.Split

main :: IO ()
main = do
   inputFile <- openFile "input/input_day2.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   let fileLines = lines fileContent ;
   putStrLn "Result for part 1 : " ;
   print ((\(a,b) -> (a,b,a*b)) (getPosition fileLines)) ;
   putStrLn "Result for part 2 : " ;
   print ((\(a,b) -> (a,b,a*b)) (getPosition2 fileLines)) ;

pairList :: [a] -> (a,a)
pairList [x,y] = (x,y)

getPosition :: [String] -> (Integer,Integer)
getPosition l = getPositionAux 0 0 (map (pairList . splitOn " ") l) where
    getPositionAux :: Integer -> Integer -> [(String,String)] -> (Integer,Integer)
    getPositionAux h v [] = (h,v)
    getPositionAux h v ((inst,num) : xs) 
        | inst == "forward" = getPositionAux (h + read num) v xs
        | inst == "down" =    getPositionAux h (v + read num) xs
        | inst == "up" =      getPositionAux h (v - read num) xs 
    
getPosition2 :: [String] -> (Integer,Integer)
getPosition2 l = getPosition2Aux 0 0 0 (map (pairList . splitOn " ") l) where
    getPosition2Aux :: Integer -> Integer -> Integer -> [(String,String)] -> (Integer,Integer)
    getPosition2Aux h v _ [] = (h,v)
    getPosition2Aux h v a ((inst,num) : xs) 
        | inst == "forward" = getPosition2Aux (h + read num) (v + a * read num) a xs
        | inst == "down" =    getPosition2Aux h v (a + read num) xs
        | inst == "up" =      getPosition2Aux h v (a - read num) xs 