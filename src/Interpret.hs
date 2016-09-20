module Interpret (interpret) where

import Expression
import Parser
import Eval

import Control.Monad (mapM_)


interpret' :: String -> Env -> [String]
interpret' [] env = []
interpret' s env =
  case parse s of
    Right (a, []) -> case eval a env of
      (Success a, newEnv) -> [toString a]
      (None, newEnv) -> []
      (Fail s, newEnv) -> [s]

    Right (a, x) -> case eval a env of
      (Success a, newEnv) -> toString a : interpret' (deleteComment x) newEnv
      (None, newEnv) -> interpret' (deleteComment x) newEnv
      (Fail s, newEnv) -> [s]

    Left s ->  ["Parser: " ++ s]

interpret :: String -> Env -> IO ()
interpret codes env = mapM_ putStrLn (interpret' (deleteComment codes) env)
