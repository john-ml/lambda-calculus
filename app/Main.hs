import LC
import Parser

import Control.Monad (forever)

main :: IO ()
main = do
  --case runParser universe "" "Type" of
  --  Right t -> print t
  --  Left e -> putStrLn $ errorBundlePretty e
  testParse "∀ a: Type 0. ∀ b: Type 0. λ f: (λ _: a. b). λ x: a. f x"
  testParse "∀ a: Type t, ∀ a: Type t1, ∀ t2: Type t + 1 ∧ t1 + (t ∧ t1), a"
  testExecute $
    "λ t: Type 0, λ t1: Type 0, λ f: Type 0, λ x: Type 0, " ++
    "(λ a: Type 0. (λ b: Type 0. (λ f: (λ _: a. b). (λ x: a. f (x))))) t t1 f x"

  testInfer "∀ A : Type 0, ∀ B : Type 0, λ a : A, λ b : B, a"

  testInfer "∀ A : Type 0, ∀ B : Type 0, λ f : (∀ a : A, B), λ x : A, f x"

  testInfer $
    "∀ A : Type 0, λ f : (∀ _ : A, A), λ x : A, f (f (f (f (f (f (f (f x)))))))"

  testInfer $
    "∀ A : Type 0, ∀ B : Type 0, ∀ C : Type 0, " ++
    "λ f : (∀ b : B, C), λ g : (∀ a : A, B), λ x : A, f (g x)"

  testInfer $
    "∀ A : Type n, ∀ B : Type n, ∀ C : Type n, " ++
    "λ f : (λ _ : A, λ _ : B, C), λ x : B, λ y : A, f y x"

  testInfer $ "λ f : (∀ A : Type n, ∀ _ : A, A), ∀ B : Type 0, λ x : B, f B x"

  forever $ getLine >>= testParse

  where
    tryEither e f = flip (either putStrLn) e f
    withParsed s = tryEither (parse s)

    testParse s = withParsed s $ \ e -> do
      print e
      putStrLn ""

    testExecute s = withParsed s $ \ e -> do
      _ <- traverse print . take 10 . execute $ e
      putStrLn ""

    testInfer s = withParsed s $ \ e -> do
      t <- infer e
      tryEither t (putStrLn . showType)
      putStrLn ""
