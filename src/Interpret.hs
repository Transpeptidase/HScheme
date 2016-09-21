module Interpret (interpret, interpret') where

import Expression
import Parser
import Eval

import Control.Monad (mapM_)


interpret' :: String -> Env -> ([String], Env)
interpret' [] env = ([], env)
interpret' s env =
  case parse s of
    Right (a, []) -> case eval a env of
      (Success a, newEnv) -> ([toString a], newEnv)
      (None, newEnv) -> ([], newEnv)
      (Fail s, newEnv) -> ([s], newEnv)

    Right (a, x) -> case eval a env of
      (Success a, newEnv) -> let (y, e) = interpret' (deleteComment x) newEnv
                             in (toString a : y, e)
      (None, newEnv) -> interpret' (deleteComment x) newEnv
      (Fail s, newEnv) -> ([s], newEnv)

    Left s ->  (["Parser: " ++ s], env)

interpret :: String -> Env -> IO ()
interpret codes = mapM_ putStrLn . fst . interpret' (deleteComment codes)
