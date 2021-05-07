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

kMeansAlgorithm :: Config -> [Pixel] -> IO ()
kMeansAlgorithm input pixelArray = do
    clusterArray <- generateClusterArray (colorsNumber input)
    return ()
