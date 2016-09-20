module BuiltIn (binOpLib, sinOpLib) where

import           Expression

arith _ op _ (Int a) (Int b) = Success $ Int (a `op` b)
arith _ _ op (Int a) (Double b)  = Success $ Double (fromInteger a `op` b)
arith _ _ op (Double a) (Int b) = Success $ Double (a `op` fromInteger b)
arith _ _ op (Double a) (Double b) = Success $ Double (a `op` b)
arith name _ _ _ _ = Fail (name ++ " expected: Int or Double")

add' = arith "add" (+) (+)
sub' = arith "sub" (-) (-)
mul' = arith "mul" (*) (*)
div' = arith "div" div (/)

mod' :: Expression -> Expression -> EvalRes
mod' (Int a) (Int b) = Success $ Int (a `mod` b)
mod' _ _ = Fail "mod expected: Int"

negate' :: Expression -> EvalRes
negate' (Int a) = Success $ Int (-a)
negate' (Double a) = Success $ Double (-a)
negate' _ = Fail "negate expexted: Int or Double"

logic :: String -> (Bool -> Bool -> Bool) -> Expression -> Expression -> EvalRes
logic _ op (Bool a) (Bool b) = Success (Bool (a `op` b))
logic name _ _ _ = Fail (name ++ " expected: Bool")

and' = logic "and" (&&)
or' = logic "or" (||)

not' :: Expression -> EvalRes
not' (Bool a) = Success (Bool (not a))
not' _ = Fail "not expexted: Bool"

compare' _ op _ (Int a) (Int b) = Success (Bool (a `op` b))
compare' _ _ op (Int a) (Double b)  = Success (Bool (fromInteger a `op` b))
compare' _ _ op (Double a) (Int b) = Success (Bool (a `op` fromInteger b))
compare' _ _ op (Double a) (Double b) = Success (Bool (a `op` b))
compare' name _ _ _ _ = Fail (name ++ " expected: Int or Double")

g' = compare' ">" (>) (>)
l' = compare' "<" (<) (<)
e' = compare' "=" (==) (==)
ge' = compare' ">=" (>=) (>=)
le' = compare' "<=" (<=) (<=)
ne' = compare' "!=" (/=) (/=)

binOpLib = [("+", add'), ("-", sub'), ("*", mul'), ("/", div'), ("%", mod')
          , ("&&", and'), ("||", or'), (">", g'), ("<", l'), ("=", e'), (">=", ge')
          , ("<=", le'), ("!=", ne')]

sinOpLib = [("-", negate'), ("!", negate')]
