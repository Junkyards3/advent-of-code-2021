{-# LANGUAGE LambdaCase #-}
import System.IO
import Data.Char (digitToInt)

main :: IO ()
main = do
   inputFile <- openFile "input/input_day3.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   let fileLines = lines fileContent ;
   putStrLn "Result for part 1 : " ;
   print (powerConsumption fileLines) ;
   putStrLn "Result for part 2 : " ;
   print (lifeSuppRating fileLines) ;
   
toNb0Nb1 :: String -> [(Integer,Integer)]
toNb0Nb1 = map (\case 
                    '0' -> (1,0) 
                    _   -> (0,1))

binStrToInt :: String -> Integer
binStrToInt s = toInteger (foldl (\acc x -> acc * 2 + digitToInt x) 0 s)

powerConsumption :: [String] -> Integer
powerConsumption l = binStrToInt (fst stringNb) *  binStrToInt (snd stringNb) where 

    stringNb :: (String,String)
    stringNb = reconstruct (nb0Nb1ByColumn l)

    reconstruct :: [(Integer,Integer)] -> (String,String)
    reconstruct [] = ("","")
    reconstruct ((nb0,nb1) : xs) 
        | nb0 <= nb1 = ('1' : fst u, '0' : snd u)
        | otherwise =  ('0' : fst u, '1' : snd u)
        where u = reconstruct xs

    nb0Nb1ByColumn :: [String] -> [(Integer,Integer)]
    nb0Nb1ByColumn l = foldr1 (zipWith (\(a,b) (c, d) -> (a+c,b+d))) (map toNb0Nb1 l)

lifeSuppRating :: [String] -> Integer
lifeSuppRating l = criteria l (>) * criteria l (<=) where 
    
    nbsFirstDigits :: [String] -> (Integer,Integer)
    nbsFirstDigits l = foldr1 (\(a,b) (c, d) -> (a+c,b+d)) (map (head . toNb0Nb1) l)

    criteria' :: [String] -> (Integer -> Integer -> Bool) -> String 
    criteria' [] _ = ""
    criteria' [s] _ = s
    criteria' l comp 
        | uncurry comp u = '0' : criteria' (map tail (filter (\ch -> '0' == head ch) l)) comp
        | otherwise = '1' : criteria' (map tail (filter (\ch -> '1' == head ch) l)) comp
        where u = nbsFirstDigits l

    criteria :: [String] -> (Integer -> Integer -> Bool) -> Integer
    criteria l comp = binStrToInt (criteria' l comp)