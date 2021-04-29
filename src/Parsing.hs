module Parsing
    ( parseArgs
    ) where

import Text.Read
import DataTypes

readPositiveInt :: String -> Maybe Int
readPositiveInt str = case readMaybe str of
        Just nb ->  if nb >= 0
                    then Just nb
                    else Nothing
        Nothing -> Nothing


parseArgs :: [String] -> Maybe Input
parseArgs [colors, convergence, file] = case readPositiveInt colors of
    Just colorsNb -> case readMaybe convergence :: Maybe Float of
        Just convergenceNb -> Just Input {
                colorsNumber = colorsNb,
                convergenceLimit = convergenceNb,
                file = file
            }
        Nothing -> Nothing
    Nothing -> Nothing
parseArgs _ = Nothing
