module Parser
  ( lexeme
  , symbol
  , word
  , name
  , var
  , simpleTerm
  , apps
  , universe
  , lambda
  , term
  , parse
  , parse'
  ) where

import LC hiding (universe)
import Data.Function (on)
import Numeric.Natural (Natural)
import Data.Char (isSpace, isDigit)
import Text.Megaparsec hiding (parse, State)
import Data.Void
import Text.Megaparsec.Char
import Control.Monad.State
import Data.List (elemIndex)
import Data.Map ((!?), Map, (!))
import qualified Data.Map as M
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

var :: Map Name Term -> [Name] -> Parser Term
var m env = do
  a <- name
  case m !? a of
    Just e -> return $ e
    Nothing ->
      case elemIndex a env of
        Just n -> return $ Var (fromIntegral n)
        Nothing -> fail $ "Variable not in scope: " ++ show a ++ " " ++ show env

lambda :: Map Name Term -> [Name] -> Parser Term
lambda m env = do
  a <- symbols ["\\", "λ"] *> name
  t <- symbol ":" *> term m env <* symbols [".", ","]
  Lam a t <$> term m (a : env)

simpleTerm :: Map Name Term -> [Name] -> Parser Term
simpleTerm m env = tryAll [lambda m env, ty, var m env, parens (term m env)]

apps :: Map Name Term -> [Name] -> Parser Term
apps m env = foldl1 App <$> some (simpleTerm m env) <?> "application"

term' :: Map Name Term -> [Name] -> Parser Term
term' m env = try (apps m env) <|> simpleTerm m env

constLambda :: Map Name Term -> [Name] -> Parser Term
constLambda m env = do
  let dummy = Name "_"
  a <- term' m env
  b <- symbols ["->", "→", "=>", "⇒"] *> term m (dummy : env)
  return $ Lam dummy a b

term :: Map Name Term -> [Name] -> Parser Term
term m env = try (constLambda m env) <|> (term' m env)

parse' :: Map Name Term -> String -> Either String Term
parse' m s = case runParser (term m [] <* eof) "" s of
  Left e -> Left $ errorBundlePretty e
  Right t -> Right t

parse :: String -> Either String Term
parse = parse' M.empty
