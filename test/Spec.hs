import Neural
import Sample
import Vector
import Test.Hspec
import Test.QuickCheck
import System.Random

samples :: Int -> [Sample Double]
samples n =
    let s = Sample (Matrix [Vector [1.0..10.0]]) (Matrix [Vector [1.0..3.0]])
    in  take n (repeat s)

positive :: Int -> Int
positive = (+1) . abs

propDims :: [Int] -> Bool
propDims xs = dims (Vector xs) == length xs

propNorm :: Float -> Bool
propNorm n =
    let v = Vector $ [n, 0, 0]
    in  abs (norm v - abs n) < 0.001

propTranspose :: Int -> Int -> Bool
propTranspose r c = 
    let m = zeroMatrix (positive r, positive c)
    in  transpose (transpose m) == m

propMultDims :: Int -> Int -> Int -> Bool
propMultDims x y z =
    let m1 = zeroMatrix (positive x, positive y)
        m2 = zeroMatrix (positive y, positive z)
        m3 = m1 * m2
    in  numColumns m3 == positive z && numRows m3 == positive x

main :: IO ()
main = hspec $ do
    describe "Vector Properties" $
        do it "propDims" $ property propDims
           it "propNorm" $ property propNorm
    describe "Matrix Properties" $
        do it "propTranspose" $ property propTranspose
           it "propMultDims" $ property propMultDims
