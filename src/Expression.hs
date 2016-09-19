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
                | Define String Expression
                | Cond [(Expression, Expression)]
                | IfElse Expression Expression Expression
                | Function String [String] Expression
                | Call Expression [Expression]
                | Clourse Env Expression
                | BuiltInBinOpFunction String Expression Expression
                | BuiltInSinOpFunction String Expression
  deriving (Show, Eq)

data EvalRes = Success Expression
             | Fail String
             | None
  deriving (Show, Eq)


toString :: Expression -> String
toString (Clourse _ (Function name _ _)) = "#<procedure:" ++ name ++ ">"
toString (Int a) = show a
toString (Double a) = show a
toString (Bool a) = if a then "true" else "false"
toString _ = "Error in toString"
