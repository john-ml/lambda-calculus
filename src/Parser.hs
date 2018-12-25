{-# LANGUAGE ViewPatterns #-}
module Parser
  ( lexeme
  , symbol
  , word
  , var
  , simpleTerm
  , apps
  , universe
  , lambda
  , term
  , parse
  ) where

import LC
import Data.Function (on)
import Numeric.Natural (Natural)
import Data.Char (isSpace, isDigit)
import Text.Megaparsec hiding (parse)
import Data.Void
import Text.Megaparsec.Char
import Control.Monad (guard)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

tryAll :: (Foldable f, MonadParsec e s m) => f (m a) -> m a
tryAll = foldr ((<|>) . try) empty

symbols :: [String] -> Parser String
symbols = tryAll . fmap symbol

parens :: Parser a -> Parser a
parens = (between `on` symbol) "(" ")"

specialChars :: String
specialChars = "\\λ().:,"

keywords :: [String]
keywords = ["forall", "Type"]

--word :: Parser String
--word = lexeme . some $ satisfy p where
--  p c = not (isSpace c) && not (c `elem` specialChars)

word :: Parser String
word = do
  s <- lexeme . some $ satisfy p
  --_ <- failure (Just (Tokens ('_' :| s))) (Set.fromList [])
  --guard . not $ s `elem` keywords
  return s
  where
    p c = not (isSpace c) && not (c `elem` specialChars)

name :: Parser Name
name = Name <$> word

natural :: Parser Natural
natural = fmap read . lexeme . some $ satisfy isDigit

universe :: Parser Universe
universe = try (UMax <$> total <*> (symbols ["/\\", "∧"] *> universe)) <|> total
  where
    lit = try (ULit <$> natural) <|> try (UVar <$> name) <|> parens universe
    total = try (UAdd <$> lit <*> (symbol "+" *> total)) <|> lit

ty :: Parser (Term Void)
ty = Type <$> (symbol "Type" *> universe)

var :: Parser (Term Void)
var = Var <$> name <?> "variable"

annotation :: Parser (Term Void)
annotation = parens $ Ann <$> term <*> (symbol ":" *> term)

simpleTerm :: Parser (Term Void)
simpleTerm = tryAll [lambda, ty, var, annotation, parens term]

apps :: Parser (Term Void)
apps = foldl1 App <$> some simpleTerm <?> "application"

lambda :: Parser (Term Void)
lambda = lam' <$> (prefix *> argument) <*> (separator *> term) where
  prefix = symbols ["\\", "λ", "∀"]
  argument = (,) <$> name <*> (symbol ":" *> term)
  separator = symbols [".", ","]
  lam' (a, t) e = Lam a t e

term :: Parser (Term Void)
term = try apps <|> simpleTerm

parse :: String -> Either String (Term Void)
parse (runParser term "" -> Left e) = Left $ errorBundlePretty e
parse (runParser term "" -> Right t) = Right t
