import Neural
import Vector
import Sample
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Environment
import System.Random

toBinary :: Int -> Int -> Matrix Double
toBinary m n =
    let l = take n (repeat 0.0) ++ [1.0] ++ take (m-n) (repeat 0.0)
    in  Matrix [Vector l]

process :: T.Text -> [Sample Double]
process t =
    let lines = map (\x -> T.split (==',') x) (T.lines t)
        toDoubles = map (\y -> read (T.unpack y) :: Double)
        toInt x = read (T.unpack x) :: Int
        feature (x:xs) = Matrix [Vector (toDoubles xs)]
        label (x:xs) = toBinary 10 (toInt x)
    in  map (\line -> Sample (feature line) (label line)) lines

main :: IO ()
main = do
    gen <- getStdGen
    args <- getArgs
    csv <- TIO.readFile (args !! 0)
    let samples = process csv
        half = (length samples) `div` 2
        pair = splitAt half samples
    print $ accuracy $ eval (randoms gen :: [Double]) (fst pair) (snd pair)
