module DataTypes
    ( Input(..)
    , Point(..)
    , Color(..)
    , Pixel(..)
    ) where

data Input = Input {
    colorsNumber :: Int,
    convergenceLimit :: Float,
    file :: String
} deriving (Show)

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show)

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
} deriving (Show)

data Pixel = Pixel {
    point :: Point,
    color :: Color
} deriving (Show)
