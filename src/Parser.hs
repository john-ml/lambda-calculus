module Parser
  ( lexeme
  , symbol
  , lit
  , word
  , hole
  , apps
  , lambda
  , term
  ) where

import LC
import Data.Function
import Data.Char (isSpace)
import Text.Megaparsec
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative hiding (some)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = (between `on` symbol) "(" ")"

lit :: Parser Term
lit = Lit . read <$> lexeme (some digitChar) <?> "literal"

word :: Parser String
word = lexeme . some $ satisfy p where
  p c = not (isSpace c) && not (c `elem` "()")

hole :: Parser Term
hole = Hole <$> word <?> "hole" where

simpleTerm :: Parser Term
simpleTerm = lambda <|> lit <|> hole <|> parens term

apps :: Parser Term
apps = foldl1 (:$) <$> some simpleTerm

lambda :: Parser Term
lambda = Λ <$> (lexeme (satisfy (`elem` "λ\\")) *> term)

term :: Parser Term
term = apps <|> simpleTerm
