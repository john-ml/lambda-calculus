{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module LC
  ( Term (..)
  , mapExpr
  , mapVars
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
  = Var Int
  | Hole String
  | Term :$ Term
  | Λ Term
  deriving (Eq, Ord)
infixl 6 :$

instance Show Term where
  show (Var a)             = show a
  show (f@(Λ _) :$ Var a)  = "(" ++ show f ++ ") " ++ show a
  show (f@(Λ _) :$ Hole a) = "(" ++ show f ++ ") " ++ a
  show (f :$ Var a)        = show f ++ " " ++ show a
  show (f :$ Hole a)       = show f ++ " " ++ a
  show (f :$ e)            = show f ++ " (" ++ show e ++ ")"
  show (Λ e)               = "λ " ++ show e ++ ""
  show (Hole s)            = s

mapExpr
  :: (Int -> Int -> Term)
  -> (Int -> String -> Term)
  -> Term
  -> Term
mapExpr f g = go 0 where
  go n (Var a)   = f n a
  go n (Hole s)  = g n s
  go n (f' :$ e) = go n f' :$ go n e
  go n (Λ e)     = Λ (go (n + 1) e)

mapVars :: (Int -> Int -> Term) -> Term -> Term
mapVars f = mapExpr f (const Hole)

mapHoles :: (Int -> String -> Term) -> Term -> Term
mapHoles g = mapExpr (const Var) g

pretty :: Map String Term -> Term -> String
pretty m = show . go 0 where
  m' = fromList $ (\ (x, y) -> (y, x)) <$> toList m
  try e handler = maybe handler Hole (m' !? e)
  go n e@(f :$ e') = try e (go n f :$ go n e')
  go n e@(Λ e') =
    case unchurch e of
      Just k  -> Hole $ "'" ++ show k
      Nothing -> try e (Λ (go (n + 1) e'))
  go _ e = e

  unchurch (Λ (Λ e)) = go' e where
    go' (Var 0)       = Just 0
    go' (Var 1 :$ e') = (1 +) <$> go' e'
    go' _             = Nothing
  unchurch _ = Nothing

adjustFree :: (Int -> Int) -> Term -> Term
adjustFree f = mapVars (\ n a -> if a >= n then Var (f a) else Var a)

substitute :: Term -> Term -> Term
substitute e = mapVars (\ n a ->
  if | a > n     -> Var (pred a)
     | a == n    -> adjustFree (+ n) e
     | otherwise -> Var a)

fill :: Map String Term -> Term -> Term
fill m = mapHoles (\ n s ->
  case m !? s of
    Just e  -> adjustFree (+ n) e
    Nothing -> Hole s)

reduce :: Term -> Term
reduce (Var a)      = Var a
reduce (Λ e' :$ e)  = substitute e e'
reduce (Var n :$ e) = Var n :$ reduce e
reduce (f :$ e)     = reduce f :$ e
reduce (Λ e)        = Λ (reduce e)
reduce (Hole s)     = Hole s

simplify :: Int -> Term -> Term
simplify steps e =
  let e'  = whileNot (\ a' a -> sizeof a' > sizeof a || a' == a) reduce e in
  let e'' = iterate reduce e' !! steps in
  if sizeof e'' < sizeof e' then e'' else e'

evaluate :: Term -> Term
evaluate = whileNot (==) reduce

sizeof :: Integral a => Term -> a
sizeof (Var _)  = 1
sizeof (Hole _) = 1
sizeof (f :$ e) = 1 + sizeof f + sizeof e
sizeof (Λ e)    = 1 + sizeof e

whileNot :: (a -> a -> Bool) -> (a -> a) -> a -> a
whileNot p f a =
  let a' = f a in
  if p a' a then a else whileNot p f a'
