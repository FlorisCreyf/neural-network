module Sample where

import Vector

data Sample a = Sample 
    { feature :: (Matrix a)
    , label :: (Matrix a)
    } deriving (Show)

data Result a = Result
    { prediction :: (Matrix a)
    , actual :: (Matrix a)
    } deriving (Eq, Show)
