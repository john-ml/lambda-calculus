module Parser
  ( Precedence
  , Name
  , MixfixToken (..)
  , Operator (..)
  , Mixfix (..)
  , Parser
  , mixfix
  , lexeme
  , lit
  , apps
  , lambda
  , term
  ) where

import LC
import Data.Function
import Text.Megaparsec
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative hiding (some)
import Data.List

type Parser = Parsec Void Name

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = (between `on` symbol) "(" ")"

lit :: Parser Term
lit = Lit . read <$> lexeme (some digitChar)

apps :: Parser Term
apps = foldl1 (:$) <$> some (lambda <|> lit <|> parens term)

lambda :: Parser Term
lambda = Λ <$> (lexeme (satisfy (`elem` "λ\\")) *> term)

term :: Parser Term
term = apps <|> lambda <|> lit <|> parens term

type Precedence = Integer
type Name = String

data MixfixToken = Exact Name | Hole Precedence
data Operator = Operator { format :: [MixfixToken], precedence :: Precedence }
data Mixfix = Mixfix { identifier :: Name, arguments :: [Mixfix] }

instance Show Operator where
  show (Operator f p) = concatMap go f ++ "(precedence = " ++ show p ++ ")" where
    go (Exact s) = "\"" ++ s ++ "\" "
    go (Hole p) = "_" ++ show p ++ " "

instance Show Mixfix where
  show (Mixfix id []) = id
  show (Mixfix id args) = "(" ++ id ++ ((' ':) . show =<< args) ++ ")" where

mixfix' :: [Operator] -> Precedence -> Parser Mixfix -> Parser Mixfix
mixfix' ops n p = go n ops' where
  ops' = sortBy (compare `on` precedence) . filter ((> n) . precedence) $ ops

  go n [] =
    try (parens (mixfix ops p))
    <|> p
    <|> fail ("Following mixfix rules failed:\n" ++ unlines (show <$> ops'))
  go n (h : t) = try (formatP h) <|> try (go n t)

  formatP (Operator [] n) = return $ Mixfix "" []
  formatP (Operator (Exact s : t) n) = do
    symbol s
    Mixfix id args <- formatP (Operator t n)
    return $ Mixfix (s ++ id) args
  formatP (Operator (Hole n' : t) n) = do
    arg <- mixfix' ops' n' p
    Mixfix id args <- formatP (Operator t n)
    return $ Mixfix ("_" ++ id) (arg : args)

mixfix :: [Operator] -> Parser Mixfix -> Parser Mixfix
mixfix ops p = mixfix' ops (pred (minimum (precedence <$> ops))) p
