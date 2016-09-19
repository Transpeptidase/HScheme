module EvalSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding (Success)
import qualified Data.Map as M

import Parser
import Expression
import Eval
import BuiltIn

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

run x = fst (eval x M.empty)

spec :: Spec
spec = do
  describe "Eval" $ do
    it "Const" $ do
      run (Int 312) `shouldBe` Success (Int 312)
      run (Double 21.4) `shouldBe` Success (Double 21.4)
      run (Bool False) `shouldBe` Success (Bool False)

    it "Cond" $ do
      run (Cond [(Bool True, Int 21)]) `shouldBe` Success (Int 21)
      run (Cond [(Bool False, Int 21), (Bool True, Double 1.2)]) `shouldBe` Success (Double 1.2)

    it "IfElse" $ do
      run (IfElse (Bool True) (Int 1) (Int 2)) `shouldBe` Success (Int 1)
      run (IfElse (Bool False) (Int 1) (Int 2)) `shouldBe` Success (Int 2)

    it "BinOp" $ do
      run (BuiltInBinOpFunction "+" (Double 1.3) (Int 5)) `shouldBe`
          Success (Double 6.3)
      run (BuiltInBinOpFunction "=" (IfElse (Bool False) (Int 1) (Int 2)) (Int 2)) `shouldBe`
          Success (Bool True)
