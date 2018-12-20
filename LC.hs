{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

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

import Data.Map hiding (foldl')
import Data.List (foldl')
import Data.Function (on)
import Data.Char

data Term
  = Lit Int
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

data StringMap a = StringMap a [StringMap a] deriving Functor

-- partial
findString (StringMap a _) "" = a
findString (StringMap _ l) (h : t) = findString (l !! ord h) t

-- infinite
strings :: StringMap String
strings = StringMap "" $ do
  c <- chr <$> [0..255]
  return $ (c :) <$> strings

data TermMap a = TermMap
  { literalMap :: [a]
  , holeMap :: StringMap a
  , appMap :: TermMap (TermMap a)
  , lamMap :: TermMap a
  } deriving Functor

-- partial
find :: TermMap a -> Term -> a
find (TermMap { literalMap = l }) (Lit k) = l !! k
find (TermMap { holeMap = m }) (Hole s) = findString m s
find (TermMap { appMap = m }) (f :$ e) = find (find m f) e
find (TermMap { lamMap = m }) (Λ e) = find m e

-- infinite
terms :: TermMap Term
terms = TermMap
  { literalMap = Lit <$> [0..]
  , holeMap = Hole <$> strings
  , appMap = (<$> terms) . (:$) <$> terms
  , lamMap = Λ <$> terms
  }

memoize :: ((Term -> a) -> Term -> a) -> Term -> a
memoize f = find (f (memoize f) <$> terms)

mapExpr
  :: (Int -> Int -> Term)
  -> (Int -> String -> Term)
  -> Term
  -> Term
mapExpr f g = go 0 where
  go n (Lit a) = f n a
  go n (Hole s) = g n s
  go n (f' :$ e) = go n f' :$ go n e
  go n (Λ e) = Λ (go (n + 1) e)

mapLits :: (Int -> Int -> Term) -> Term -> Term
mapLits f = mapExpr f (const Hole)

mapHoles :: (Int -> String -> Term) -> Term -> Term
mapHoles g = mapExpr (const Lit) g

pretty :: Map String Term -> Term -> String
pretty m = show . go 0 where
  m' = fromList $ (\ (x, y) -> (y, x)) <$> toList m
  try e handler = maybe handler Hole (m' !? e)
  go n e@(f :$ e') = try e (go n f :$ go n e')
  go n e@(Λ e') =
    case unchurch e of
      Just k -> Hole $ "'" ++ show k
      Nothing -> try e (Λ (go (n + 1) e'))
  go _ e = e

  unchurch (Λ (Λ e)) = go' e where
    go' (Lit 0) = Just 0
    go' (Lit 1 :$ e) = (1 +) <$> go' e
    go' _ = Nothing
  unchurch _ = Nothing

adjustFree :: (Int -> Int) -> Term -> Term
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

--reduce :: Term -> Term
--reduce = memoize reduce' where
--  --ret = id --simplify 20
--  reduce' rec (Lit a) = ret $ Lit a
--  reduce' rec (Λ e' :$ e) = ret $ substitute e e'
--  reduce' rec (Lit n :$ e) = ret $ Lit n :$ rec e
--  reduce' rec (f :$ e) = ret $ rec f :$ e
--  reduce' rec (Λ e) = ret $ Λ (rec e)
--  reduce' rec (Hole s) = ret $ Hole s

simplify :: Int -> Term -> Term
simplify steps e =
  let (f ||| g) a b = f a b || g a b in
  let e' = whileNot (((>) `on` sizeof) ||| (==)) reduce e in
  let e'' = iterate reduce e' !! steps in
  if sizeof e'' < sizeof e' then e'' else e'

evaluate :: Term -> Term
evaluate = whileNot (==) reduce

sizeof :: Integral a => Term -> a
sizeof (Lit a) = 1
sizeof (Hole s) = 1
sizeof (f :$ e) = 1 + sizeof f + sizeof e
sizeof (Λ e) = 1 + sizeof e

whileNot :: (a -> a -> Bool) -> (a -> a) -> a -> a
whileNot p f a =
  let a' = f a in
  if p a' a then a else whileNot p f a'
