module Main where

import System.Environment
import System.Exit
import qualified Data.Map as Map
import qualified TSP as TSP
import qualified TSPData as TSPData

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage   >> exit
parse [s]    = do
    dat <- readFile s
    let g = TSPData.loadCitiesDataAsGraph $ lines dat
    TSP.run g

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

main :: IO ()
main = do
    args <- getArgs
    parse args

