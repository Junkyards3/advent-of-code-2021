{-# LANGUAGE LambdaCase #-}
import System.IO
import Data.Char (digitToInt)

main :: IO ()
main = do
   inputFile <- openFile "input_day3.txt" ReadMode ;
   fileContent <- hGetContents inputFile ;
   let fileLines = lines fileContent ;
   putStrLn "Result for part 1 : " ;
   print (gammatdeltaRate fileLines) ;
   putStrLn "Result for part 2 : " ;
   print (lifeSuppRating fileLines) ;
   
toDigits :: String -> [(Integer,Integer)]
toDigits = map (\case 
                    '0' -> (1,0) 
                    _   -> (0,1))

toDec :: String -> Integer
toDec s = toInteger (foldl (\acc x -> acc * 2 + digitToInt x) 0 s)

gammatdeltaRate :: [String] -> Integer
gammatdeltaRate l = toDec stringNb *  toDec (invert stringNb) where 

    stringNb :: String 
    stringNb = reconstruct (nbs_digits l)


    
    invert :: String -> String 
    invert [] = ""
    invert ('0' : xs) = '1' : invert xs
    invert ('1' : xs) = '0' : invert xs

    reconstruct :: [(Integer,Integer)] -> String 
    reconstruct [] = ""
    reconstruct ((nb0,nb1) : xs) 
        | nb0 <= nb1 = '1' : reconstruct xs
        | otherwise = '0' : reconstruct xs


    nbs_digits :: [String] -> [(Integer,Integer)]
    nbs_digits l = foldr1 (zipWith (\(a,b) (c, d) -> (a+c,b+d))) (map toDigits l)

lifeSuppRating :: [String] -> Integer
lifeSuppRating l = criteria l (>) * criteria l (<=) where 
    
    firstDigits :: String -> (Integer,Integer)
    firstDigits s = head (toDigits s)

    nbsFirstDigits :: [String] -> (Integer,Integer)
    nbsFirstDigits l = foldr1 (\(a,b) (c, d) -> (a+c,b+d)) (map firstDigits l)

    criteria' :: [String] -> (Integer -> Integer -> Bool) -> String 
    criteria' [] _ = ""
    criteria' ("" : xs) _ = ""
    criteria' [s] _ = s
    criteria' l comp 
        | uncurry comp u = '0' : criteria' (map tail (filter (\ch -> '0' == head ch) l)) comp
        | otherwise = '1' : criteria' (map tail (filter (\ch -> '1' == head ch) l)) comp
        where u = nbsFirstDigits l

    criteria :: [String] -> (Integer -> Integer -> Bool) -> Integer
    criteria l comp = toDec (criteria' l comp)