module Main where

import Repl

import qualified Data.Map as M

main :: IO ()
main = repl M.empty
