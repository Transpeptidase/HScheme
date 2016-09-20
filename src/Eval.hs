module Eval where

import           BuiltIn
import           Expression

import           Data.List  (find)
import qualified Data.Map   as M

eval :: Expression -> Env -> (EvalRes, Env)

eval (Var s) env = case M.lookup s env of
  Just v -> (Success v, env)
  Nothing -> (Fail (s ++ ": undefined"), env)

eval a@(Int _) env = (Success a, env)
eval a@(Double _) env = (Success a, env)
eval a@(Bool _) env = (Success a, env)
eval a@Closure {} env = (Success a, env)

eval (Define s ex) env =
  case fst (eval ex env) of
    a@(Success r) -> (None, M.insert s r env)
    a -> (a, env)

eval (Cond []) env = (None, env)
eval (Cond ((c, ex):rest)) env =
  case fst (eval c env) of
    Success (Bool True) -> eval ex env
    Success (Bool False) -> eval (Cond rest) env
    _  -> (Fail "Cond need Bool", env)

eval (IfElse cond e1 e2) env =
  case fst (eval cond env) of
    Success (Bool True) -> eval e1 env
    Success (Bool False) -> eval e2 env
    _  -> (Fail "If need Bool", env)


eval (BuiltInBinOpFunction s e1 e2) env =
  case lookup s binOpLib of
    Just f ->
      let v1 = fst (eval e1 env)
          v2 = fst (eval e2 env)
      in case (v1, v2) of
        (Success a, Success b) -> (f a b, env)
        (Fail a, _) -> (Fail a, env)
        (_, Fail b) -> (Fail b, env)
        _           -> (Fail "Args can't be none", env)
    _ -> (Fail (s ++ ": undefined"), env)

eval (BuiltInSinOpFunction s e) env =
  case lookup s sinOpLib of
    Just f -> case fst (eval e env) of
      (Success a) -> (f a, env)
      (Fail a) -> (Fail a, env)
      _        -> (Fail "Args can't be none", env)
    _ -> (Fail (s ++ ": undefined"), env)


eval a@Function{} env =
  (Success (Closure env a), env)

eval (Call funcE args) env =
  case fst (eval funcE env) of
    Success c@(Closure fEnv (Function fname argnames body)) ->
      if length args /= length argnames
      then (Fail argNumDisMatch, env)
      else
        let argsV = map (\x -> fst (eval x env)) args
        in case find isNotSuccess argsV of
             Nothing -> let argMap = zipWith (\a (Success b) -> (a, b)) argnames argsV
                            funEnv = if null fname then foldr (uncurry M.insert) fEnv argMap
                                     else M.insert fname c $ foldr (uncurry M.insert) fEnv argMap
                        in (fst (eval body funEnv), env)
             Just a@(Fail _) -> (a, env)
             _  -> (Fail "Something be none", env)
      where
        argNumDisMatch =
          let funS = if null fname then "lambda function" else fname
          in funS ++ " : arity mismatch" ++ "\nexpected: " ++ show (length argnames)
             ++ "\ngiven: " ++ show (length args)
        isNotSuccess Success{} = False
        isNotSuccess _         = True
    a@Fail{} -> (a, env)
    _ -> (Fail "This expression is not a procedure", env)
