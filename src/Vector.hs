module Vector where

import Data.Semigroup
import System.Random

-- vector definitions
newtype Vector a = Vector [a] deriving (Show)

instance Eq a => Eq (Vector a) where
    Vector xs == Vector ys = xs == ys

instance Functor Vector where
    fmap f (Vector xs) = Vector $ map f xs

instance Semigroup (Vector a) where
    (Vector xs) <> (Vector ys) = Vector $ xs ++ ys

instance Monoid (Vector a) where
    mempty = Vector []
    mappend (Vector xs) (Vector ys) = Vector $ xs ++ ys

instance Num a => Num (Vector a) where
    (Vector xs) + (Vector ys) = Vector $ zipWith (+) xs ys
    (Vector xs) - (Vector ys) = Vector $ zipWith (-) xs ys
    (Vector xs) * (Vector ys) = Vector $ zipWith (*) xs ys
    abs v = fmap abs v 
    fromInteger x = Vector [fromInteger x]
    negate v = fmap negate v
    signum v = fmap signum v

instance Fractional a => Fractional (Vector a) where
    (Vector xs) / (Vector ys) = Vector $ zipWith (/) xs ys
    fromRational x = Vector [fromRational x]

instance Foldable Vector where
    foldMap f (Vector []) = mempty
    foldMap f (Vector (xs)) = foldMap f xs 

randVector :: RandomGen g => g -> (Double, Double) -> Int -> Vector Double
randVector gen range size = Vector $ list
    where list = take size (randomRs range gen :: [Double])

dims :: Vector a -> Int
dims (Vector xs) = length xs

norm :: Floating a => Vector a -> a
norm (Vector xs) = sqrt $ getSum $ mconcat $ map Sum $ map (**2) xs

dot :: Num a => Vector a -> Vector a -> a
dot (Vector xs) (Vector ys) = getSum $ mconcat $ map Sum (zipWith (*) xs ys)

svmult :: Num a => a -> Vector a -> Vector a
svmult s v = fmap (*s) v

-- matrix definitions
newtype Matrix a = Matrix [Vector a] deriving (Show, Eq)

instance Functor Matrix where
    fmap f (Matrix xs) = Matrix (map (fmap f) xs)

instance Foldable Matrix where
    foldMap f (Matrix []) = mempty
    foldMap f (Matrix vs) = foldMap f (mconcat vs)

instance Num a => Num (Matrix a) where
    m1 + m2 = madd m1 m2
    m1 - m2 = msub m1 m2
    m1 * m2 = mmult m1 m2
    abs m = fmap abs m
    negate m = fmap negate m
    signum m = fmap signum m
    fromInteger x = Matrix [Vector [fromInteger x]]

vectorToMatrix :: Vector a -> Matrix a
vectorToMatrix x = Matrix [x]

zeroMatrix :: (Int, Int) -> Matrix Double
zeroMatrix shape = 
    let row = Vector $ take (snd shape) (repeat 0)
    in  Matrix $ take (fst shape) (repeat row)

initMatrix :: [Double] -> (Int, Int) -> Matrix Double
initMatrix nums size =
    let add vs ns = (Vector $ take (snd size) ns):vs
        rows 0 vs _ = vs
        rows i vs ns = rows (i-1) (add vs ns) (drop (snd size) ns)
    in  Matrix $ rows (fst size) [] nums

numRows :: Matrix a -> Int
numRows (Matrix xs) = length xs

numColumns :: Matrix a -> Int
numColumns (Matrix []) = 0
numColumns (Matrix (x:xs)) = dims x

transpose :: Matrix a -> Matrix a
transpose (Matrix ((Vector []):_)) = Matrix []
transpose (Matrix vs) = 
    let heads vs = Vector $ map (\(Vector (n:ns)) -> n) vs
        tails vs = transpose $ Matrix (map (\(Vector (n:ns)) -> Vector ns) vs)
        add v (Matrix vs) = Matrix (v:vs)
    in  add (heads vs) (tails vs)

madd :: Num a => Matrix a -> Matrix a -> Matrix a
madd (Matrix xs) (Matrix ys) = Matrix $ zipWith (+) xs ys

msub :: Num a => Matrix a -> Matrix a -> Matrix a
msub (Matrix xs) (Matrix ys) = Matrix $ zipWith(-) xs ys

mmult :: Num a => Matrix a -> Matrix a -> Matrix a
mmult (Matrix xs) m2 = 
    let rows (Matrix rows) = rows
        mult xs y = Vector $ map (dot y) xs
    in  Matrix $ map (mult (rows (transpose m2))) xs

smmult :: Num a => a -> Matrix a -> Matrix a
smmult s m = fmap (*s) m
