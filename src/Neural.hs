module Neural where

import qualified Data.List as L
import Vector
import Sample
import System.Random

accuracy :: [Result Double] -> Double
accuracy rs = realToFrac (sum xs) / (L.genericLength xs)
    where xs = map (\x -> if prediction x == actual x then 1.0 else 0.0) rs

eval :: [Double] -> [Sample Double] -> [Sample Double] -> [Result Double]
eval randoms trainingSamples testSamples =
    let weights = train randoms trainingSamples
        prediction sample = asLabel (feedforward sample weights)
        result sample = Result (prediction sample) (label sample)
    in  map result testSamples

train :: [Double] -> [Sample Double] -> Matrix Double
train randoms samples =
    let cols (sample:samples) = numColumns $ label sample
        rows (sample:samples) = numColumns $ feature sample
        weights = initMatrix randoms (rows samples, cols samples)
    in  epochs 10 weights 0.5 samples

epochs :: Real a => Int -> Matrix a -> a -> [Sample a] -> Matrix a
epochs (-1) weights _ _ = weights
epochs count weights rate samples =
    let iteration = length samples - 1
        weights' = epoch iteration weights rate samples
    in  epochs (count - 1) weights' rate samples

epoch :: Real a => Int -> Matrix a -> a -> [Sample a] -> Matrix a
epoch (-1) weights _ _ = weights
epoch index weights rate samples =
    let weights' = update rate weights (samples !! index)
    in  epoch (index - 1) weights' rate samples

feedforward :: Real a => Sample a -> Matrix a -> Matrix a
feedforward sample weights = (feature sample) * weights

activate :: Real a => Matrix a -> Matrix a
activate = fmap (\x -> if x > 0 then 1 else 0)

update :: Real a => a -> Matrix a -> Sample a -> Matrix a
update rate weights sample =
    let output = feedforward sample weights
        activation = activate output
        input = transpose $ feature sample
        delta = rate `smmult` (input * (label sample - activation))
    in  weights + delta

asLabel :: Matrix Double -> Matrix Double
asLabel m = fmap (\x -> if x-max == 0.0 then 1.0 else 0.0) m
    where max = maximum m 
