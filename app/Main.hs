module Main where

import Repl
import Interpret
import BasicLib

import qualified Data.Map as M
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
  then do
    -- enter repl
    putStrLn "Welcome to HScheme !"
    repl initEnv
  else do
    -- interpret file
    contents <- readFile $ head args
    interpret contents initEnv
