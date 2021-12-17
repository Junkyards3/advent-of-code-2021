import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Maybe (isJust)

data Time = Fin Int | Infinity deriving (Eq,Show)

instance Ord Time where
    Fin a `compare` Fin b = a `compare` b
    Infinity `compare` Fin b = GT
    Fin a `compare` Infinity = LT
    _ `compare` _ = EQ

type Speed = Int
type Pos = Int
type Interval = (Time, Time)

main :: IO ()
main = do
    inputFile <- openFile "input/input_day16.txt" ReadMode ;
    fileContent <- hGetContents inputFile ;
    putStrLn "Result for part 1 : " ;
    let (xmin,xmax,ymin,ymax) = process ;
    let t = possibleSpeeds xmin xmax ymin ymax ;
    let ym =  maximum . map snd $ t ;
    print $ ym*(ym+1) `div` 2 ;
    putStrLn "Result for part 2 : " ;
    print $ length t ;
    print t ;

process :: (Pos,Pos,Pos,Pos)
process = (70,125,-159,-121)

ffl1 :: Int -> Int -> Float
ffl1 i1 i2 =  (v0fs - sqrt (v0fs ** 2 - 8*xminf))/2
    where v0fs = fromIntegral $ 1 + 2*i1
          xminf = fromIntegral i2

ffl2 :: Int -> Int -> Float
ffl2 i1 i2 =  (v0fs + sqrt (v0fs ** 2 - 8*xminf))/2
    where v0fs = fromIntegral $ 1 + 2*i1
          xminf = fromIntegral i2

intervalV0 :: Pos -> Pos -> (Speed,Speed)
intervalV0 xmin xmax = (ceiling $ (sqrt (1 + 8*fromIntegral xmin) - 1)/ 2,xmax)

intervalNX :: Speed -> Pos -> Pos -> Interval
intervalNX v0 xmin xmax = (Fin . ceiling $ ffl1 v0 xmin,
    if v0*(v0+1) <= 2*xmax then Infinity else Fin . floor $ ffl1 v0 xmax)

intervalW0 :: Pos -> Pos -> (Speed,Speed)
intervalW0 ymin ymax = (ymin,max (abs ymin) (abs ymax) * 2)

intervalNY :: Speed -> Pos -> Pos -> (Interval,Interval)
intervalNY w0 ymin ymax = ((Fin . ceiling $ ffl1 w0 ymin, Fin . floor $ ffl1 w0 ymax),
    (Fin . ceiling $ ffl2 w0 ymax, Fin . floor $ ffl2 w0 ymin))

intersect :: Interval -> Interval -> Bool
intersect (t1,t2) (t1p,t2p) = u1 <= u2
    where u1 = max t1 t1p
          u2 = min t2 t2p

possibleSpeeds :: Pos -> Pos -> Pos -> Pos -> [(Speed,Speed)]
possibleSpeeds xmin xmax ymin ymax = [(v0,w0) | v0 <- [v0min..v0max], w0 <- [w0min..w0max],
    let {ix = intervalNX v0 xmin xmax ; (iy1,iy2) = intervalNY w0 ymin ymax} in
    ix `intersect` iy1 || ix `intersect` iy2]
    where
    (v0min,v0max) = intervalV0 xmin xmax
    (w0min,w0max) = intervalW0 ymin ymax