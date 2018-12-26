{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module LC
  ( Universe
  , Term
  , Universe' (..)
  , Term' (..)
  , Name (..)
  , fresh
  , names
  , showTerm
  , showType
  , flatMap
  , unique
  , substitute
  , occursFree
  , termBind
  , reduce
  , step
  , execute
  , evaluate
  , infer
  ) where

import           Numeric.Natural (Natural)
import           Data.Bifunctor (Bifunctor, first, second)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map, (!?))
import qualified Data.Map as Map
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Functor (($>))

data Universe' a
  = UVar a
  | UMax (Universe' a) (Universe' a)
  | UAdd (Universe' a) (Universe' a)
  | ULit Natural
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Universe' a) where
  show (UVar a) = show a
  show (UMax a b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (UAdd a b) = show a ++ " + " ++ show b
  show (ULit n) = show n

data Term' a b
  = Hole a
  | Var b
  | App (Term' a b) (Term' a b)
  | Lam b (Term' a b) (Term' a b)
  | Ann (Term' a b) (Term' a b)
  | Type (Universe' b)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Universe' where
  pure = UVar
  a <*> b = a >>= \ f -> b >>= \ x -> return $ f x

instance Monad Universe' where
  UVar a >>= f = f a
  UMax a b >>= f = UMax (a >>= f) (b >>= f)
  UAdd a b >>= f = UAdd (a >>= f) (b >>= f)
  ULit a >>= _ = ULit a

flatMap :: (b -> Term' a b) -> (b -> Universe' b) -> Term' a b -> Term' a b
flatMap _ _ (Hole a)    = Hole a
flatMap f _ (Var a)     = f a
flatMap f g (App a b)   = App (flatMap f g a) (flatMap f g b)
flatMap f g (Lam a t e) = Lam a (flatMap f g t) (flatMap f g e)
flatMap f g (Ann e t)   = Ann (flatMap f g e) (flatMap f g t)
flatMap _ g (Type u)    = Type (u >>= g)

termBind :: Term' a b -> (b -> Term' a b) -> Term' a b
m `termBind` f = flatMap f pure m

occursFree :: Eq b => b -> Term' a b -> Bool
occursFree b = any (== b)

showExpr :: (Eq b, Show a, Show b) => Bool -> Term' a b -> String
showExpr isType = \case
  Hole a    -> "?" ++ show a
  Var a     -> show a
  App f e   -> goLamPar f ++ " (" ++ go e ++ ")"
  Lam a t e ->
    let (p, q) = (isType, occursFree a e) in
    if | p && q     -> "∀ " ++ show a ++ " : " ++ go t ++ ", " ++ go e
       | p && not q -> goVarPar t ++ " -> " ++ go e
       | otherwise  -> "λ " ++ show a ++ " : " ++ showExpr True t ++ ". " ++ go e
  Ann e t   -> go e ++ " : " ++ go t
  Type u    -> "Type " ++ show u
  where
    go = showExpr isType

    goLamPar f@(Lam _ _ _) = "(" ++ go f ++ ")"
    goLamPar f = go f

    goVarPar v@(Var _) = go v
    goVarPar v = "(" ++ go v ++ ")"

showTerm :: (Eq b, Show a, Show b) => Term' a b -> String
showTerm = showExpr False

showType :: (Eq b, Show a, Show b) => Term' a b -> String
showType = showExpr True

instance (Eq b, Show a, Show b) => Show (Term' a b) where
  show = showTerm

instance Bifunctor Term' where
  first f (Hole a)    = Hole (f a)
  first _ (Var b)     = Var b
  first f (App f' e)  = App (first f f') (first f e)
  first f (Lam a t e) = Lam a (first f t) (first f e)
  first f (Ann e t)   = Ann (first f e) (first f t)
  first _ (Type u)    = Type u

  second = fmap

-- suffixes should be infinite
fresh' :: Semigroup a => [a] -> [a] -> [a]
fresh' prefixes suffixes = prefixes ++ (flip (<>) <$> suffixes <*> prefixes)

names :: Ord b => Term' a b -> Set b
names = foldr Set.insert Set.empty

-- assumes infinite [c]
unique' :: (Ord b, Ord c) => Term' a b -> State ([c], Bimap b c) (Term' a c)
unique' = \case
  Hole a    -> return $ Hole a
  Var a     -> Var <$> rename a where
  App f e   -> App <$> unique' f <*> unique' e
  Ann e t   -> Ann <$> unique' e <*> unique' t
  Type u    -> Type <$> traverse rename u
  Lam a t e -> do
    new <- gensym a
    Lam new <$> unique' t <*> unique' e
  where
    rename a = do
      (_, m) <- get
      maybe (gensym a) return $ Bimap.lookup a m
    gensym a = do
      (l, m) <- get
      let new : l' = filter (not . flip Bimap.memberR m) l
      put (l', Bimap.insert a new m)
      return new

-- assumes all bindings are unique
substitute :: Eq b => b -> Term' a b -> Term' a b -> Term' a b
substitute a e' = flip termBind $ \ a' -> if a == a' then e' else Var a'

newtype Name = Name { unName :: String } deriving (Eq, Ord, Semigroup)
instance Show Name where show = unName

type Universe = Universe' Name
type Term a = Term' a Name

-- infinite
fresh :: [Name]
fresh = fresh' (Name . pure <$> ['a' .. 'z']) (Name . show <$> [0..])

-- make bindings unique; alpha-equivalent expressions will always yield same results
unique :: Term a -> Term a
unique = flip evalState (fresh, Bimap.empty) . unique'

type EvalM = Writer Any

reduce :: Term a -> EvalM (Term a)
reduce (Hole a) = pure $ Hole a
reduce (Var a) = pure $ Var a
reduce (App (Lam a _ e) e') = tell (Any True) $> substitute a e' e
reduce (App f e) = App <$> reduce f <*> pure e
reduce (Lam a t e) = Lam a <$> reduce t <*> reduce e
reduce (Ann e t) = Ann <$> reduce e <*> reduce t
reduce (Type u) = pure $ Type u

step :: Term a -> Term a
step = fst . runWriter . reduce 

execute :: Term a -> [Term a]
execute = iterate step

evaluate :: Term a -> Term a
evaluate (runWriter . reduce -> (e', Any changed))
  | changed = evaluate e'
  | otherwise = e'

data StateType a b = StateType
  { bindings :: Map b (Term' a b)
  , inequalities :: [(Universe' b, Universe' b)]
  }

mapBindings :: (Map c (Term' a c) -> Map c (Term' b c)) -> StateType a c -> StateType b c
mapBindings f (StateType a b) = StateType (f a) b

mapInequalities
  :: ([(Universe' b, Universe' b)] -> [(Universe' b, Universe' b)])
  -> StateType a b -> StateType a b
mapInequalities f (StateType a b) = StateType a (f b)

type CheckM' a b = ExceptT String (State (StateType a b))

match' :: (Show a, Show b, Eq a, Ord b) => Term' a b -> Term' a b -> CheckM' a b ()
match' s t = do
  ok <- go s t Bimap.empty
  if ok then
    return ()
  else
    throwError $ "Can't match '" ++ show s ++ "' with '" ++ show t ++ "'"
  where
    go (Hole a) (Hole a') _ = return $ a == a'
    go (Var a) (Var a') m = return $ a == a' || Bimap.pairMember (a, a') m
    go (App f e) (App f' e') m = (&&) <$> go f f' m <*> go e e' m
    go (Lam a t' e) (Lam a' t'' e') m =
      let m' = Bimap.insert a a' m in
      (&&) <$> go t' t'' m' <*> go e e' m'
    go (Ann e t') (Ann e' t'') m = (&&) <$> go e e' m <*> go t' t'' m
    go (Type u) (Type u') m = go' u u' m
    go _ _ _ = return False

    go' u u' _ = do
      modify . mapInequalities $ ((u, u') :)
      return True

check' :: (Show a, Show b, Eq a, Ord b) => Term' a b -> Term' a b -> CheckM' a b ()
check' (Hole a) t = throwError $ "Found hole: " ++ show a ++ " : " ++ show t
check' e t = infer' e >>= match' t

infer' :: (Show a, Show b, Eq a, Ord b) => Term' a b -> CheckM' a b (Term' a b)
infer' (Hole a) = throwError $ "Found hole: " ++ show a
infer' (Var a) = do
  context <- bindings <$> get
  case context !? a of
    Just t' -> return t'
    Nothing -> throwError $ "Variable not in scope: " ++ show a
infer' (App f e) = do
  tf <- infer' f
  case tf of
    Lam a t e' -> check' e t $> substitute a e e'
    _ -> throwError $ "Non-functional construction: " ++ show f ++ " : " ++ show tf
infer' (Lam a t e) = do
  _ <- infer' t
  modify . mapBindings $ Map.insert a t
  Lam a t <$> infer' e
infer' (Ann e t) = check' e t $> t
infer' (Type u) = return $ Type u

infer :: (Show a, Eq a) => Term a -> Either String (Term a)
infer = flip evalState (StateType Map.empty []) . runExceptT . infer' . unique
