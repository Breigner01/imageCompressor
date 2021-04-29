module DataTypes
    ( Input(..)
    , Pixel(..)
    ) where

data Input = Input {
    colorsNumber :: Int,
    convergenceLimit :: Float,
    file :: String
} deriving (Show)

data Pixel = Pixel {
    x :: Int,
    y :: Int,
    r :: Int,
    g :: Int,
    b :: Int
} deriving (Show)
