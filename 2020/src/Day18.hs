module Day18
  (day18part1, day18part2)
  where

import           Common
import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Expr = NumVal Int
  | Sum Expr Expr
  | Product Expr Expr
  deriving Show

skipSpaces :: Parser a -> Parser a
skipSpaces p = space *> p <* space

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

binary :: Char -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary sym f = InfixL (f <$ skipSpaces (char sym))

termParser :: Parser Expr
termParser = choice
  [ skipSpaces $ parens (skipSpaces exprParser)
  , NumVal <$> skipSpaces intParser
  ]

exprParser :: Parser Expr
exprParser = makeExprParser termParser operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[binary '*' Product , binary '+' Sum]]

calculate :: Expr -> Int
calculate (NumVal x)    = x
calculate (Sum x y)     = calculate x + calculate y
calculate (Product x y) = calculate x * calculate y

day18part1 :: String -> String
day18part1 = show . sum . map calculate . readListOf exprParser

day18part2 :: String -> String
day18part2 _ = ""
