{-# LANGUAGE LambdaCase #-}
import LC
import Parser

import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Control.Monad.State
import Text.Megaparsec hiding (parse)
import Data.Void
import Data.Char
import Data.List
import Data.Function
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

test :: IO ()
test = do
  testParse "λ a: Type 0. λ b: Type 0. λ f: (λ _: a. b). λ x: a. f x"
  testParse "λ a: Type t, λ a: Type t1, λ t2: Type t + 1 ∧ t1 + (t ∧ t1), a"
  testExecute $
    "λ t: Type 0, λ t1: Type 0, λ f: Type 0, λ x: Type 0, " ++
    "(λ a: Type 0. λ b: Type 0. λ f: a -> b. λ x: a. f x) t t1 f x"
  testInfer "λ A : Type 0, λ B : Type 0, λ a : A, λ b : B, a"
  testInfer "λ A : Type 0, λ B : Type 0, λ f : A -> B, λ x : A, f x"
  testInfer $
    "λ A : Type 0, λ f : A -> A, λ x : A, f (f (f (f (f (f (f (f x)))))))"
  testInfer $
    "λ A : Type 0, λ B : Type 0, λ C : Type 0, " ++
    "λ f : B -> C, λ g : A -> B, λ x : A, f (g x)"
  testInfer $
    "λ A : Type n, λ B : Type n, λ C : Type n, " ++
    "λ f : A -> B -> C, λ x : B, λ y : A, f y x"
  testInfer $ "λ f : (λ A : Type n, A -> A), λ B : Type 0, λ x : B, f B x"
  testInfer $ "λ f : (λ A : Type n, A -> A), λ B : Type n + 1, λ x : B, f B x"
  testInfer $ "λ f : (λ A : Type 1, A -> A), λ B : Type 0, λ x : B, f (Type 0) B"

  testInfer $ "λ f : (λ A : Type 0, A), λ g : (λ A : Type 0, A), f g"

tryEither :: Either String b -> (b -> IO ()) -> IO ()
tryEither e f = flip (either putStrLn) e f

withParsed :: String -> (Term -> IO ()) -> IO ()
withParsed s = tryEither (parse s)

testParse :: String -> IO ()
testParse s = withParsed s $ \ e -> do
  print e
  putStrLn ""

testExecute :: String -> IO ()
testExecute s = withParsed s $ \ e -> do
  _ <- traverse print . take 10 . execute $ e
  putStrLn ""

testInfer :: String -> IO ()
testInfer s = withParsed s $ \ e -> do
  t <- infer e
  tryEither t (putStrLn . showType)
  putStrLn ""

testRun :: String -> IO ()
testRun s = withParsed s $ \ e -> do
  et <- run e
  tryEither et $ \ (e', t) ->
    putStrLn $ "(" ++ showTerm e' ++ ") : (" ++ showType t ++ ")"
  putStrLn ""

type ReplM = StateT (Map Name Term, [String]) IO

prompt :: String -> ReplM String
prompt s = liftIO $ putStr s *> hFlush stdout *> getLine

type Parser = Parsec Void String
data Command = Query Term | Binding Name Term | Use [String]

userInput :: Map Name Term -> Parser Command
userInput m = try directive <|> try definition <|> Query <$> term m [] <* eof where
  directive = symbol ":" *> word >>= \case
    "use" -> Use <$> some word
    s -> fail $ "Unknown command '" ++ s ++ "'"
  definition = Binding <$> name <* symbol ":=" <*> term m [] <* eof

blocks :: String -> [String]
blocks = (concat <$>) . groupBy ((<) `on` indent) . lines where
  indent = length . takeWhile isSpace

runLine :: String -> ReplM ()
runLine "" = return ()
runLine ('#' : _) = return ()
runLine s = do
  (bindings, _) <- get
  case runParser (userInput bindings) "" s of
    Left e -> liftIO . putStrLn $ errorBundlePretty e
    Right (Query e) -> liftIO $ do
     et <- run e
     tryEither et $ \ (e', t) ->
       putStrLn $ "(" ++ showTerm e' ++ ") : (" ++ showType t ++ ")"
    Right (Binding s' e) -> do
      modify . first $ M.insert s' e
    Right (Use files) -> do
      forM_ files $ (mapM_ runLine . blocks =<<) . lift . readFile
      modify $ second (++ files)

repl :: ReplM ()
repl = forever $ do
  (_, loaded) <- get
  runLine =<< (prompt $ unwords loaded ++ "> ")

main :: IO ()
main = test *> void (runStateT repl (M.empty, []))
