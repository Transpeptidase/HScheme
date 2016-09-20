module Main where

import Repl
import Interpret

import qualified Data.Map as M
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
  then repl M.empty
  else do
    contents <- readFile $ head args
    interpret contents M.empty
