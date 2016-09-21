module Expression
    (
    Expression(..)
  , EvalRes(..)
  , Env
  , toString
    ) where

import qualified Data.Map as M

type Env = M.Map String Expression

data Expression = Var String
                | Int Integer
                | Double Double
                | Bool Bool
                | Char Char
                | List [Expression]
                | Define String Expression
                | Cond [(Expression, Expression)]
                | IfElse Expression Expression Expression
                | Function String [String] Expression
                | Call Expression [Expression]
                | Closure Env Expression
                | BuiltInBinOpFunction String Expression Expression
                | BuiltInSinOpFunction String Expression
  deriving (Show, Eq)

data EvalRes = Success Expression
             | Fail String
             | None
  deriving (Show, Eq)

toString :: Expression -> String
toString (Closure _ (Function name _ _)) = "#<procedure:" ++ name ++ ">"
toString (Int a) = show a
toString (Double a) = show a
toString (Bool a) = if a then "true" else "false"
toString (Char c) = show c
toString (List []) = "[]"
toString (List s) =
  if all isChar s then show $ map getC s
  else '[' : ' ' : foldr (\ a b -> toString a ++ (' ' : b)) "]" s
  where
    isChar (Char _) = True
    isChar _ = False
    getC (Char a) = a

toString _ = "Error in toString"
