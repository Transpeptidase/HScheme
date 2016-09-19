module ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Parser
import Expression

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

run = runParser parser

ans x = Right (x, "")

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Int" $ do
      run " -90  " `shouldBe` ans (Int (-90))
      run "-0312  " `shouldBe` ans (Int (-312))
      run " 123123" `shouldBe` ans (Int 123123)

    it "Double" $ do
      run " 00.21  " `shouldBe` ans (Double 0.21)
      run " -00.21  " `shouldBe` ans (Double (-0.21))
      run "     21.32" `shouldBe` ans (Double 21.32)

    it "Bool" $ do
      run " false  " `shouldBe` ans (Bool False)
      run "true  " `shouldBe` ans (Bool True)

    it "Var" $ do
      run "  Yu_sa" `shouldBe` ans (Var "Yu_sa")
      run "  a" `shouldBe` ans (Var "a")
      run " truee" `shouldBe` ans (Var "truee")

    it "Define Value" $ do
      run " (define x y) " `shouldBe` ans (Define "x" (Var "y"))
      run " ( define xs true ) " `shouldBe` ans (Define "xs" (Bool True))
      run " ( define x_s -31.31 ) " `shouldBe` ans (Define "x_s" (Double (-31.31)))

    it "Define Function" $ do
      run " (define (fun) x)"  `shouldBe` ans (Define "fun" (Function "fun" [] (Var "x")))
      run " (  define( f x y zz ) 21.232)" `shouldBe` ans (Define "f" (Function "f" ["x", "y", "zz"] (Double 21.232)))

    it "Cond" $ do
      run " (cond (true wq) (0 21))" `shouldBe` ans (Cond [(Bool True, Var "wq"), (Int 0, Int 21)])
      run "( cond  (false (cond (0 1) ) ))" `shouldBe` ans (Cond [(Bool False, Cond [(Int 0, Int 1)])])

    it "IfElse" $ do
      run "(if true 21 23 )" `shouldBe` ans (IfElse (Bool True) (Int 21) (Int 23))
      run "( if (cond (1 3)) sa true)" `shouldBe` ans (IfElse (Cond [(Int 1, Int 3)]) (Var "sa") (Bool True))

    it "Lambda" $ do
      run " (lambda () x)" `shouldBe` ans (Function "" [] (Var "x"))
      run " (lambda (x y zxs) (if true w -21.4))" `shouldBe`
        ans (Function "" ["x", "y", "zxs"] (IfElse (Bool True) (Var "w") (Double (-21.4))))

    it "Call" $ do
      run " ( wq x 5 6)" `shouldBe` ans (Call (Var "wq") [Var "x", Int 5, Int 6])
      run "(w_e)" `shouldBe` ans (Call (Var "w_e") [])
      run " ((lambda () x) yy u)" `shouldBe` ans (Call (Function "" [] (Var "x")) [Var "yy", Var "u"])

    it "BinOp" $ do
      run " (+ 4 5)" `shouldBe` ans (BuiltInBinOpFunction "+" (Int 4) (Int 5))
      run " (<= 4 6)" `shouldBe` ans (BuiltInBinOpFunction "<=" (Int 4) (Int 6))

    it "SinOp" $ do
      run " (  ! true)" `shouldBe` ans (BuiltInSinOpFunction "!" (Bool True))
      run " (-  (if true 1 2))" `shouldBe` ans (BuiltInSinOpFunction "-" (IfElse (Bool True) (Int 1) (Int 2)))
