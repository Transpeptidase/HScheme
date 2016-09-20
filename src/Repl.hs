module Repl (repl) where

import           Eval
import           Expression
import           Parser

import qualified Data.Map    as M
import           System.Exit (exitSuccess)
import           System.IO   (hFlush, stdout)

repl :: Env -> IO ()
repl env = do
  putStr "==> "
  hFlush stdout
  s <- getLine
  if all (== ' ') s then repl env
  else if filter (/= ' ') s == "(exit)" then exitSuccess
  else case parse s of
    Right (a, []) -> case eval a env of
      (Success a, newEnv) -> do
        putStrLn $ toString a
        repl newEnv
      (None, newEnv) -> repl newEnv
      (Fail s, newEnv) -> do
        putStrLn s
        repl newEnv

    Right (_, x) -> do
      putStrLn ("Parser: has redundance characters \"" ++ x ++ "\"")
      repl env

    Left s -> do
      putStrLn ("Parser: " ++ s)
      repl env
