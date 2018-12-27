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
import Text.Megaparsec hiding (parse, State)
import Data.Void
import Text.Megaparsec.Char
import Control.Monad.State
import Data.List (elemIndex)
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

ty :: Parser Term
ty = Type <$> (symbol "Type" *> universe)

var :: [Name] -> Parser Term
var env = do
  a <- name
  case elemIndex a env of
    Just n -> return $ Var (fromIntegral n)
    Nothing -> fail $ "Variable not in scope: " ++ show a ++ " " ++ show env

lambda :: [Name] -> Parser Term
lambda env = do
  a <- symbols ["\\", "λ"] *> name
  t <- symbol ":" *> term env <* symbols [".", ","]
  Lam a t <$> term (a : env)

simpleTerm :: [Name] -> Parser Term
simpleTerm env = tryAll [lambda env, ty, var env, parens (term env)]

apps :: [Name] -> Parser Term
apps env = foldl1 App <$> some (simpleTerm env) <?> "application"

term' :: [Name] -> Parser Term
term' env = try (apps env) <|> simpleTerm env

constLambda :: [Name] -> Parser Term
constLambda env = do
  let dummy = Name "_"
  a <- term' env
  b <- symbols ["->", "→"] *> term (dummy : env)
  return $ Lam dummy a b

term :: [Name] -> Parser Term
term env = try (constLambda env) <|> (term' env)

parse :: String -> Either String Term
parse s = case runParser (term [] <* eof) "" s of
  Left e -> Left $ errorBundlePretty e
  Right t -> Right t
