module Repl where

import Eval
import Expression
import Parser

import qualified Data.Map as M
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

-- repl :: String -> Env -> (EvalRes, Env)
-- repl s env =
--   case runParser parser s of
--     Right (a, []) -> eval a env
--     Right (_, x) -> (Fail ("Parser: has redundance characters \"" ++ x ++ "\""), env)
--     Left s -> (Fail ("Parser: " ++ s), env)
--
-- replStrList :: [String] -> [(EvalRes, Env)]
-- replStrList xs = aux xs M.empty
--   where
--     aux [] env = []
--     aux (x:xs) env = let a@(ans, e) = repl x env
--                      in a : aux xs e

repl :: Env -> IO ()
repl env = do
  putStr "==> "
  hFlush stdout
  s <- getLine
  if all (== ' ') s then repl env
  else if filter (/= ' ') s == "(exit)" then exitSuccess
  else case runParser parser s of
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
