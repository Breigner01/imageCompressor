module Parsing
    ( parseArgs
    , parseFile
    ) where

import Text.Read
import GHC.IO.Handle.Text
import GHC.IO.Handle.Types
import GHC.IO.Handle
import System.Directory
import DataTypes

readPositiveInt :: String -> Maybe Int
readPositiveInt str = case readMaybe str of
        Just nb ->  if nb >= 0
                    then Just nb
                    else Nothing
        Nothing -> Nothing

readColor :: String -> Maybe Int
readColor str = case readMaybe str of
        Just nb ->  if nb >= 0 && nb < 256
                    then Just nb
                    else Nothing
        Nothing -> Nothing

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

charInString :: Char -> String -> Bool
charInString c (s:str)  | c == s = True
                        | otherwise = charInString c str
charInString c _ = False

stringToken :: String -> String -> String
stringToken (s:str) delim   | charInString s delim = []
                            | otherwise = s : stringToken str delim
stringToken [] _ = []

stringToArray :: String -> String -> [String]
stringToArray (s:str) delim | not (charInString s delim) = stringToArray str delim
                            | otherwise = case stringToken str delim of
                                "" -> stringToArray str delim
                                string -> string : stringToArray str delim
stringToArray [] _ = []

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
