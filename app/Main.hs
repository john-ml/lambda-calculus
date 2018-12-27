import LC
import Parser

import System.IO (hFlush, stdout)
import Control.Monad (forever)

main :: IO ()
main = test *> repl

repl :: IO a
repl = forever $ do
  putStr "> " *> hFlush stdout *> getLine >>= testInfer

test :: IO ()
test = do
  testParse "∀ a: Type 0. ∀ b: Type 0. λ f: (λ _: a. b). λ x: a. f x"
  testParse "∀ a: Type t, ∀ a: Type t1, ∀ t2: Type t + 1 ∧ t1 + (t ∧ t1), a"
  testExecute $
    "λ t: Type 0, λ t1: Type 0, λ f: Type 0, λ x: Type 0, " ++
    "(λ a: Type 0. λ b: Type 0. λ f: a -> b. λ x: a. f x) t t1 f x"
  testInfer "∀ A : Type 0, ∀ B : Type 0, λ a : A, λ b : B, a"
  testInfer "∀ A : Type 0, ∀ B : Type 0, λ f : A -> B, λ x : A, f x"
  testInfer $
    "∀ A : Type 0, λ f : A -> A, λ x : A, f (f (f (f (f (f (f (f x)))))))"
  testInfer $
    "∀ A : Type 0, ∀ B : Type 0, ∀ C : Type 0, " ++
    "λ f : B -> C, λ g : A -> B, λ x : A, f (g x)"
  testInfer $
    "∀ A : Type n, ∀ B : Type n, ∀ C : Type n, " ++
    "λ f : A -> B -> C, λ x : B, λ y : A, f y x"
  testInfer $ "λ f : (∀ A : Type n, A -> A), ∀ B : Type 0, λ x : B, f B x"
  testInfer $ "λ f : (∀ A : Type n, A -> A), ∀ B : Type n + 1, λ x : B, f B x"
  testInfer $ "λ f : (∀ A : Type 1, A -> A), ∀ B : Type 0, λ x : B, f (Type 0) B"

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
