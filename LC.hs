{-# LANGUAGE MultiWayIf #-}

module LC
  ( Term (..)
  , forExpr
  , forLits
  , forHoles
  , substitute
  , fill
  , reduce
  , evaluate
  ) where

import Data.Map

data Term
  = Lit Integer
  | Hole String
  | Term :$ Term
  | Λ Term
  deriving (Eq, Ord)
infixl 6 :$

instance Show Term where
  show (Lit a) = show a
  show (f@(Λ _) :$ Lit a) = "(" ++ show f ++ ") " ++ show a
  show (f :$ Lit a) = show f ++ " " ++ show a
  show (f :$ e) = show f ++ " (" ++ show e ++ ")"
  show (Λ e) = "λ " ++ show e ++ ""
  show (Hole s) = s

forExpr
  :: (Integer -> Integer -> Term)
  -> (Integer -> String -> Term)
  -> Term
  -> Term
forExpr f g = go 0 where
  go n (Lit a) = f n a
  go n (Hole s) = g n s
  go n (f' :$ e) = go n f' :$ go n e
  go n (Λ e) = Λ (go (n + 1) e)

forLits :: (Integer -> Integer -> Term) -> Term -> Term
forLits f = forExpr f (const Hole)

forHoles :: (Integer -> String -> Term) -> Term -> Term
forHoles g = forExpr (const Lit) g

adjustFree :: (Integer -> Integer) -> Term -> Term
adjustFree f = forLits (\ n a -> if a >= n then Lit (f a) else Lit a)

substitute :: Term -> Term -> Term
substitute e = forLits (\ n a ->
  if | a > n     -> Lit (pred a)
     | a == n    -> adjustFree (+ n) e
     | otherwise -> Lit a)

fill :: Map String Term -> Term -> Term
fill m = forHoles (\ n s ->
  case m !? s of
    Just e -> adjustFree (+ n) e
    Nothing -> Hole s)

reduce :: Term -> Term
reduce (Lit a) = Lit a
reduce (Λ e' :$ e) = substitute e e'
reduce (Lit n :$ e) = Lit n :$ reduce e
reduce (f :$ e) = reduce f :$ e
reduce (Λ e) = Λ (reduce e)
reduce (Hole s) = Hole s

evaluate :: Term -> Term
evaluate e =
  let e' = reduce e in
  if e' == e then e else evaluate e'
