module KMeansAlgorithm where

import GHC.Float
import System.Random
import DataTypes
import DataComparison

generateClusterArray :: Int -> IO [Cluster]
generateClusterArray 0 = return []
generateClusterArray nb = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)
    clusterArray <- generateClusterArray (nb - 1)
    return (Cluster {centroid = Color {r = int2Float r, g = int2Float g,
        b = int2Float b}, pixelArray = []} : clusterArray)

newClusterArray :: [Cluster] -> [Cluster]
newClusterArray [] = []
newClusterArray (cluster:clusterArray) =
    newCluster cluster : newClusterArray clusterArray

newCluster :: Cluster -> Cluster
newCluster cluster@(Cluster c@(Color r g b) _) =
    Cluster {centroid = Color {r = r, g = g, b = b}, pixelArray = []}

computeDistance :: Color -> Color -> Float
computeDistance c1@(Color r1 g1 b1) c2@(Color r2 g2 b2) =
    sqrt ((r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2))

pixelToCluster :: Pixel -> Int -> Int -> Float -> [Cluster] -> Int
pixelToCluster _ _ is _ [] = is
pixelToCluster px@(Pixel pt c1) i is d (cl@(Cluster c2 pxArr):clArr) =
    if nd < d
    then pixelToCluster px (i + 1) (i + 1) nd clArr
    else pixelToCluster px (i + 1) is d clArr
    where
        nd = computeDistance c1 c2

emplacePixel :: Pixel -> Int -> [Cluster] -> [Cluster]
emplacePixel px 0 (cl@(Cluster _ pxArr):clArr) =
    cl {pixelArray = px : pxArr} : clArr
emplacePixel px i (cl:clArr) = cl : emplacePixel px (i - 1) clArr

colorSum :: [Pixel] -> Color -> Color
colorSum [] c = c
colorSum (px@(Pixel _ c@(Color r1 g1 b1)):pxArr) cs@(Color r2 g2 b2) =
    colorSum pxArr cs {r = r1 + r2, g = g1 + g2, b = b1 + b2}

computeCentroid :: [Pixel] -> Color
computeCentroid pxArr =
    Color {r = r / len, g = g / len, b = b / len}
    where
        c@(Color r g b) = colorSum pxArr Color {r = 0, g = 0, b = 0}
        len = int2Float (length pxArr)

computeAllCentroids :: [Cluster] -> [Cluster]
computeAllCentroids [] = []
computeAllCentroids (cl@(Cluster _ px):clArr) =
    cl {centroid = computeCentroid px} : computeAllCentroids clArr

algorithm :: [Cluster] -> [Pixel] -> [Cluster]
algorithm clArr (px:pxArr) = undefined

kMeansAlgorithm :: Config -> [Pixel] -> IO ()
kMeansAlgorithm input pixelArray = do
    clusterArray <- generateClusterArray (colorsNumber input)
    return ()
