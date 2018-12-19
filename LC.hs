{-# LANGUAGE MultiWayIf #-}

module LC
  ( Term (..)
  , mapExpr
  , mapLits
  , mapHoles
  , pretty
  , substitute
  , fill
  , reduce
  , sizeof
  , simplify
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
  show (f@(Λ _) :$ Hole a) = "(" ++ show f ++ ") " ++ a
  show (f :$ Lit a) = show f ++ " " ++ show a
  show (f :$ Hole a) = show f ++ " " ++ a
  show (f :$ e) = show f ++ " (" ++ show e ++ ")"
  show (Λ e) = "λ " ++ show e ++ ""
  show (Hole s) = s

mapExpr
  :: (Integer -> Integer -> Term)
  -> (Integer -> String -> Term)
  -> Term
  -> Term
mapExpr f g = go 0 where
  go n (Lit a) = f n a
  go n (Hole s) = g n s
  go n (f' :$ e) = go n f' :$ go n e
  go n (Λ e) = Λ (go (n + 1) e)

mapLits :: (Integer -> Integer -> Term) -> Term -> Term
mapLits f = mapExpr f (const Hole)

mapHoles :: (Integer -> String -> Term) -> Term -> Term
mapHoles g = mapExpr (const Lit) g

pretty :: Map String Term -> Term -> String
pretty m = show . go 0 where
  m' = fromList $ (\ (x, y) -> (y, x)) <$> toList m
  try e handler = maybe handler Hole (m' !? e)
  go n e@(f :$ e') = try e (go n f :$ go n e')
  go n e@(Λ e') = try e (Λ (go (n + 1) e'))
  go _ e = e

adjustFree :: (Integer -> Integer) -> Term -> Term
adjustFree f = mapLits (\ n a -> if a >= n then Lit (f a) else Lit a)

substitute :: Term -> Term -> Term
substitute e = mapLits (\ n a ->
  if | a > n     -> Lit (pred a)
     | a == n    -> adjustFree (+ n) e
     | otherwise -> Lit a)

fill :: Map String Term -> Term -> Term
fill m = mapHoles (\ n s ->
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

sizeof :: Integral a => Term -> a
sizeof (Lit a) = 1
sizeof (Hole s) = 1
sizeof (f :$ e) = 1 + sizeof f + sizeof e
sizeof (Λ e) = 1 + sizeof e

simplify :: Term -> Term
simplify e =
  let e' = iterate reduce e !! 100 in
  if sizeof e' < sizeof e then e' else e

evaluate :: Term -> Term
evaluate e =
  let e' = reduce e in
  if e' == e then e else evaluate e'
