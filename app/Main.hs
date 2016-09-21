module Main where

import Repl
import Interpret

import qualified Data.Map as M
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
  then do
    -- enter repl
    putStrLn "Welcome to HScheme !"
    repl M.empty
  else do
    -- interpret file
    contents <- readFile $ head args
    interpret contents M.empty
