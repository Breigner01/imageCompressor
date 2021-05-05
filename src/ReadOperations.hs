module ReadOperations
    ( readPositiveInt
    , readColor
    ) where

import Text.Read

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
