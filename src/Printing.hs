module Printing
    ( printClusterArray
    ) where

import DataTypes

printClusterArray :: [Cluster] -> IO ()
printClusterArray [] = return ()
printClusterArray (cl:clArr) =
    printCluster cl >>
    printClusterArray clArr

printCluster :: Cluster -> IO ()
printCluster cl@(Cluster c pxArr) =
    putStrLn "--" >>
    printColor c >>
    putStrLn "-" >>
    printPixelArray pxArr

printColor :: Color -> IO ()
printColor c@(Color r g b) =
    putStrLn ('(' : (show (round r)) ++ "," ++ (show (round g)) ++
        "," ++ (show (round b)) ++ ")")

printPixelArray :: [Pixel] -> IO ()
printPixelArray [] = return ()
printPixelArray (px:pxArr) =
    printPixel px >>
    printPixelArray pxArr

printPixel :: Pixel -> IO ()
printPixel px@(Pixel p c) =
    printPoint p >>
    printColor c

printPoint :: Point -> IO ()
printPoint p@(Point x y) =
    putStr ('(' : (show x) ++ "," ++ (show y) ++ ") ")
