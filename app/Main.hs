module Main where

import System.Environment
import System.Exit
import Parsing

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just input -> print input >> exitWith ExitSuccess
        Nothing -> putStrLn "Invalid Arguments" >> exitWith (ExitFailure 84)
