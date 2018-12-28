{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module LC
  ( Name (..)
  , Term' (..)
  , Term
  , Universe' (..)
  , Universe
  , showTerm
  , showType
  , showTermPretty
  , showTypePretty
  , traverseTerm
  , mapTerm
  , joinTerm
  , bindTerm
  , len
  , (↑)
  , occursFree
  , substitute
  , step'
  , step
  , execute
  , evaluate
  , universe
  , infer
  , run
  ) where

import Numeric.Natural (Natural)
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.List (genericIndex, intercalate)
import Data.Bifunctor (first, second, bimap)
import Data.Functor (($>))
import Data.Function (on)
import Data.SBV hiding (showType)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

newtype Name = Name { unName :: String } deriving (Eq, Ord)
instance Show Name where show (Name s) = s

data Universe' a
  = UVar a
  | ULit Natural
  | UMax (Universe' a) (Universe' a)
  | UAdd (Universe' a) (Universe' a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)
type Universe = Universe' Name

data Term' u a
  = Var a
  | App (Term' u a) (Term' u a)
  | Lam Name (Term' u a) (Term' u a)
  | Type (Universe' u)
  deriving (Eq, Ord)
type Term = Term' Name Natural

instance Show a => Show (Universe' a) where
  show (UVar a) = show a
  show (UMax a b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (UAdd a b) = show a ++ " + " ++ show b
  show (ULit n) = show n

instance (Integral a, Show a, Ord u, Show u) => Show (Term' u a) where
  show = showTerm

nameless :: Term' u a -> Term' u a
nameless (Var a) = Var a
nameless (App f e) = App (nameless f) (nameless e)
nameless (Lam _ t e) = Lam (Name "_") (nameless t) (nameless e)
nameless (Type u) = Type u

showExpr
  :: (Integral a, Ord u, Show u, Show a)
  => Bool -> [Name] -> Map (Term' u a) Name -> Term' u a -> String
showExpr isType l m = \case
  e | nameless e `M.member` m -> show $ m ! nameless e
  Var a -> if a >= len l then "?" ++ show a else show $ genericIndex l a
  App f e -> goLamPar f ++ " (" ++ go e ++ ")"
  Lam a t e ->
    let (p, q) = (isType, occursFree 0 e) in
    if | p && q     -> "λ " ++ show a ++ " : " ++ go t ++ ", " ++ goNest a e
       | p && not q -> goVarPar t ++ " -> " ++ goNest a e
       | not p && q -> "λ " ++ show a ++ " : " ++ showExpr True l m t ++ ". " ++ goNest a e
       | otherwise  -> goVarPar t ++ " => " ++ goNest a e
  Type u -> "Type " ++ show u
  where
    go = showExpr isType l m

    goLamPar f@(Lam _ _ _) = "(" ++ go f ++ ")"
    goLamPar f = go f

    goVarPar v@(Var _) = go v
    goVarPar v = "(" ++ go v ++ ")"

    goNest a e = showExpr isType (a : l) m e

invert
  :: (Integral a, Ord u, Show u, Show a) => Map Name (Term' u a) -> Map (Term' u a) Name
invert = M.fromList . map (\ (s, e) -> (nameless e, s)) . M.toList

showTermPretty
  :: (Integral b, Ord a, Show a, Show b) => Map Name (Term' a b) -> Term' a b -> String
showTermPretty m = showExpr False [] (invert m)

showTypePretty
  :: (Integral b, Ord a, Show a, Show b) => Map Name (Term' a b) -> Term' a b -> String
showTypePretty m = showExpr True [] (invert m)

showTerm :: (Integral b, Ord a, Show a, Show b) => Term' a b -> String
showTerm = showTermPretty M.empty

showType :: (Integral b, Ord a, Show a, Show b) => Term' a b -> String
showType = showTypePretty M.empty

traverseTerm :: Applicative f => ([Name] -> a -> f b) -> Term' u a -> f (Term' u b)
traverseTerm f = go [] where
  go l (Var a) = Var <$> f l a
  go l (App g e) = App <$> go l g <*> go l e
  go l (Lam s t e) = Lam s <$> go l t <*> go (s : l) e
  go _ (Type u) = pure $ Type u

mapTerm :: ([Name] -> a -> b) -> Term' u a -> Term' u b
mapTerm f = runIdentity . traverseTerm (\ l a -> Identity $ f l a)

joinTerm :: Term' u (Term' u a) -> Term' u a
joinTerm (Var a) = a
joinTerm (App f e) = App (joinTerm f) (joinTerm e)
joinTerm (Lam s t e) = Lam s (joinTerm t) (joinTerm e)
joinTerm (Type u) = Type u

bindTerm :: Term' u a -> ([Name] -> a -> Term' u b) -> Term' u b
bindTerm m f = joinTerm $ mapTerm f m

len :: (Foldable f, Integral b) => f a -> b
len = foldr (const (+ 1)) 0

(↑) :: Integral a => Term' u a -> a -> Term' u a
e ↑ k = flip mapTerm e $ \ l a -> if a >= len l then a + k else a

occursFree :: Integral a => a -> Term' u a -> Bool
occursFree k = getAny . execWriter . traverseTerm (\ l a ->
  if a == k + len l then tell (Any True) else return ())

substitute :: Integral a => Term' u a -> Term' u a -> Term' u a
substitute e' e = bindTerm e $ \ l a ->
  let n = len l in
  if | a > n -> Var (a - 1)
     | a == n -> e' ↑ n
     | otherwise -> Var a

step' :: Integral a => Term' u a -> State Bool (Term' u a)
step' (Var a)              = return $ Var a
step' (App (Lam _ _ e) e') = substitute e' e <$ put True
step' (App f e)            = App <$> step' f <*> step' e
step' (Lam a t e)          = Lam a <$> step' t <*> step' e
step' (Type u)             = return $ Type u

step :: Integral a => Term' u a -> Term' u a
step e = evalState (step' e) False

execute :: Integral a => Term' u a -> [Term' u a]
execute e@(flip runState False . step' -> (e', changed))
  | changed = e : execute e'
  | otherwise = [e, e']

evaluate :: Integral a => Term' u a -> Term' u a
evaluate = last . execute

type Constraint = (Universe, Universe)
type Env = [(Name, Term)]
type CheckM = ExceptT String (StateT (Env, Set Constraint) IO)

(!?) :: (Show a, Integral a) => [Term' u a] -> a -> CheckM (Term' u a)
l !? k
  | k < len l = return $ genericIndex l k ↑ (k + 1)
  | otherwise = throwError $ "Variable not in scope: " ++ show k

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines

showConstraints :: Set Constraint -> String
showConstraints = intercalate "\n" . map (leq . bimap show show) . S.toList where
  leq (l, r) = l ++ " ≤ " ++ r

universe :: Term -> CheckM Universe
universe = \case
  Var a -> universe =<< (!? a) =<< map snd . fst <$> get
  App a b -> umax <$> universe a <*> universe b
  Lam a t e -> do
    ut <- universe t
    modify $ first ((a, t) :)
    ue <- universe e
    modify $ first tail
    if occursFree 0 e then
      return $ if ut == ue then inc ut else umax (inc ut) ue
    else
      return $ umax ut ue
  Type u -> return u
  where
    inc = UAdd (ULit 1)
    umax a b
      | a == b = a
      | otherwise = UMax a b

simplify :: Set Constraint -> Set Constraint
simplify = S.filter (not . uncurry (==))

toSym :: Set Constraint -> Symbolic SBool
toSym constraints = do
  vars' <- sequence vars
  let nats = (literal 0 .<=) . snd <$> M.toList vars'
  let clauses = uncurry ((.<=) `on` sym vars') <$> S.toList constraints
  return $ bAnd (nats ++ clauses)
  where
    names = S.toList . foldMap (foldMap S.singleton . uncurry UAdd) $ constraints
    vars = M.fromList $ zip names (exists . unName <$> names)
    sym _     (ULit n) = literal (fromIntegral n :: Integer)
    sym vars' (UVar s) = vars' ! s
    sym vars' (UMax a b) = smax (sym vars' a) (sym vars' b)
    sym vars' (UAdd a b) = sym vars' a + sym vars' b

subtype :: Term -> Term -> CheckM Bool
subtype (Var a) (Var a') = return $ a == a'
subtype (App f e) (App f' e') = (&&) <$> subtype f f' <*> subtype e e'
subtype (Lam _ t e) (Lam _ t' e') = (&&) <$> subtype t t' <*> subtype e e'
subtype t (Type u') = do
  u <- universe t
  modify . second $ simplify . (S.insert (u, u'))
  constraints <- snd <$> get
  result <- liftIO . sat $ toSym constraints
  case result of
    SatResult (Satisfiable _ _) -> return True
    _ -> throwError $
           "Universe consistency check failed:\n" ++ indent (show result) ++
           "\nConstraints were:\n" ++ indent (showConstraints constraints)
subtype _ _ = return False

checkSubtype :: Term -> Term -> CheckM ()
checkSubtype s t = subtype s t >>= \case
  True -> return ()
  False -> do
    env <- fst <$> get
    throwError $
      "Can't match '" ++ showExpr True (fst <$> env) M.empty s ++
      "' with '" ++ showExpr True (fst <$> env) M.empty t ++ "'"

infer' :: Term -> CheckM Term
infer' (Var a) = (!? a) =<< map snd . fst <$> get
infer' (App f e) = do
  tf <- infer' f
  case evaluate tf of
    Lam _ t e' -> (evaluate <$> infer' e >>= (`checkSubtype` t)) $> substitute e e'
    tf' -> throwError $ "Non-functional construction: " ++ show f ++ " : " ++ show tf'
infer' (Lam a t e) =
  Lam a t <$> (infer' t *> modify (first ((a, t) :)) *> infer' e <* modify (first tail))
infer' (Type u) = return $ Type (UAdd u (ULit 1))

infer :: Term -> IO (Either String Term)
infer = flip evalStateT ([], S.empty) . runExceptT . infer'

run :: Term -> IO (Either String (Term, Term))
run e = do
  a <- infer e
  case a of
    Left s -> return $ Left s
    Right t -> return $ Right (evaluate e, t)
