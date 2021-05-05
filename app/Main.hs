module Main where

import System.Environment
import System.Exit
import System.IO
import DataTypes
import Parsing

help :: IO ()
help =  putStrLn "USAGE: ./imageCompressor n e IN\n" >>
        putStrLn "\tn\tnumber of colors in the final image" >>
        putStrLn "\te\tconvergence limit" >>
        putStrLn "\tIN\tpath to the file containing the colors of the pixels"

main :: IO ()
main = do
    args <- getArgs
    parsedArgs <- parseArgs args
    case parsedArgs of
        Just input -> do
            content <- readFile (file input)
            case parseFile (lines content) of
                Just pixelArray -> print pixelArray >> exitWith ExitSuccess
                Nothing -> putStrLn "Invalid File" >> exitWith (ExitFailure 84)
        Nothing -> help >> exitWith (ExitFailure 84)
