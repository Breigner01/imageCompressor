module Parsing
    ( parseArgs
    , parseFile
    ) where

import Text.Read
import System.Directory
import StringOperations
import ReadOperations
import DataTypes

parseArgs :: [String] -> IO (Maybe Input)
parseArgs [colors, convergence, file] = case readPositiveInt colors of
        Just colorsNb -> case readMaybe convergence :: Maybe Float of
            Just convergenceNb -> do
                fileExistence <- doesFileExist file
                if fileExistence then return (Just Input{colorsNumber=colorsNb,
                    convergenceLimit = convergenceNb, file = file})
                else return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
parseArgs _ = return Nothing

retrieveColors :: Int -> Int -> [String] -> Maybe Pixel
retrieveColors x y (rs:gs:bs:s) =
    case readColor rs of
        Just r -> case readColor gs of
            Just g -> case readColor bs of
                Just b -> Just Pixel {point = Point {x = x, y = y},
                                      color = Color {r = r, g = g, b = b}}
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing


retrieveStringContent :: String -> Maybe Pixel
retrieveStringContent str =
    case readPositiveInt xs of
        Just x -> case readPositiveInt ys of
            Just y -> retrieveColors x y (rs:gs:bs:s)
            Nothing -> Nothing
        Nothing -> Nothing
    where (xs:ys:rs:gs:bs:s) = stringToArray str "(), \n"

parseFile :: [String] -> Maybe [Pixel]
parseFile (line:lines) = case retrieveStringContent line of
        Just pixel -> case parseFile lines of
            Just pixelArray -> Just (pixel : pixelArray)
            Nothing -> Nothing
        Nothing -> Nothing
parseFile [] = Just []
