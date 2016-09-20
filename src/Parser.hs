module Parser (deleteComment, parse) where

import           Control.Monad (ap, liftM, unless)
import           Data.List     (foldr1)
import           Expression

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  (<*>) = ap
  pure = return

instance Monad Parser where
  fail s = Parser $ \_ -> Left s
  return a = Parser $ \x -> Right (a, x)
  m >>= k = Parser $ \x ->
    case runParser m x of
      Left msg -> Left msg
      Right (a, s) -> runParser (k a) s

combineParser :: Parser a -> Parser a -> Parser a
combineParser p q = Parser $ \x ->
  case runParser p x of
    Right (a, s) -> Right (a, s)
    _            -> runParser q x

parseChar :: Parser Char
parseChar = Parser $ \x ->
  case x of
    c:cs-> Right (c, cs)
    [] -> Left "It's ended !"

sta :: (Char -> Bool) -> Parser Char
sta f = do
  x <- parseChar
  if f x then return x else fail "It's error!"

char :: Char -> Parser Char
char c = sta (==c)

star :: Parser a -> Parser [a]
star p = combineParser (plus p) (return [])

-- reject empty
plus :: Parser a -> Parser [a]
plus p =  do
  x <- p
  xs <- star p
  return (x:xs)

-- onr or zero
ques :: Char -> Parser String
ques t = Parser $ \x ->
  case x of
    c:cs -> if c == t then Right ([c], cs) else Right ([], x)
    [] -> Right ([], x)


oneOf :: [Parser a] -> Parser a
oneOf = foldr1 combineParser

oneOfChar :: String -> Parser Char
oneOfChar cs =  sta (`elem` cs)

space :: Parser Char
space = oneOfChar " \t\n"

digit = oneOfChar "0123456789"

digits :: Parser String
digits = plus digit

alphabet :: Parser Char
alphabet = oneOfChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

operators :: Parser String
operators = plus $oneOfChar "+-*/%<>=!"

identifier :: Parser String
identifier = do
  a <- oneOf [alphabet, char '_']
  rest <- star (oneOf [alphabet, digit, char '_', char '?', char '-'])
  return (a : rest)

string :: String -> Parser ()
string s = do
  a <- identifier
  unless (s == a) $ fail ("can't match " ++ s  ++ " !")

inBracket :: Parser a -> Parser a
inBracket p = do
  star space >> char '(' >> star space
  x <- p
  star space >> char ')' >> star space
  return x

parseInt :: Parser Expression
parseInt = do
  sign <- ques '-'
  a <- digits
  return (Int (read (sign ++ a)))

parseVarAndBool :: Parser Expression
parseVarAndBool = do
  ide <- identifier
  case ide of
    "true" -> return (Bool True)
    "else" -> return (Bool True)
    "false" -> return (Bool False)
    _ -> return (Var ide)

parseDouble :: Parser Expression
parseDouble = do
  sign <- ques '-'
  a <- digits
  char '.'
  b <- digits
  return (Double (read (sign ++ a ++ "." ++ b)))

parseDefineValue :: Parser Expression
parseDefineValue = do
  char '('
  star space
  string "define"
  plus space
  ide <- identifier
  plus space
  expression <- parser
  char ')'
  return (Define ide expression)

parseDefineFunction :: Parser Expression
parseDefineFunction = do
  char '('
  star space
  string "define"
  ides <- inBracket $ plus (identifier >>= \x -> star space >> return x)
  expression <- parser
  char ')'
  return (Define (head ides) (Function (head ides) (tail ides) expression))

parseCond :: Parser Expression
parseCond = do
  char '('
  star space
  string "cond"
  ans <- plus $ inBracket $ do
    cond <- parser
    e <- parser
    return (cond, e)
  char ')'
  return (Cond ans)

parseIfElse :: Parser Expression
parseIfElse = do
  char '('
  star space
  string "if"
  plus space
  e1 <- parser
  e2 <- parser
  e3 <- parser
  char ')'
  return (IfElse e1 e2 e3)

parseLambda :: Parser Expression
parseLambda = do
  char '('
  star space
  string "lambda"
  ides <- inBracket $ star (identifier >>= \x -> star space >> return x)
  expression <- parser
  char ')'
  return (Function "" ides expression)

parseCall :: Parser Expression
parseCall = do
  char '('
  e1 <- parser
  args <- star parser
  char ')'
  return (Call e1 args)

parseBinOp :: Parser Expression
parseBinOp = do
  char '('
  star space
  op <- operators
  args1 <- parser
  args2 <- parser
  char ')'
  return (BuiltInBinOpFunction op args1 args2)

parseSinOp :: Parser Expression
parseSinOp = do
  char '('
  star space
  op <- operators
  args1 <- parser
  char ')'
  return (BuiltInSinOpFunction op args1)

parseComment :: Parser ()
parseComment = do
  star space
  char ';'
  star (sta (/= '\n'))
  star space
  return ()

parseList =
  [parseDouble, parseInt, parseDefineValue, parseDefineFunction, parseCond
  ,parseVarAndBool, parseIfElse, parseLambda, parseCall
  ,parseBinOp, parseSinOp]

parser :: Parser Expression
parser = do
  star space
  x <- oneOf parseList
  star space
  return x

deleteComment :: String -> String
deleteComment s =
  case runParser (star parseComment) s of
    Right (_, rest) -> rest
    _ -> error "Parser Comment"

parse :: String -> Either String (Expression, String)
parse = runParser parser
