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
  , infer
  , normalize
  , toSym
  ) where

import Numeric.Natural (Natural)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.List (genericIndex, intercalate)
import Data.Bifunctor (first, second, bimap)
import Data.Functor (($>))
import Data.Function (on)
import Debug.Trace
import Data.SBV hiding (showType)
import Data.Set (Set)
import Data.Map ((!), Map)
import qualified Data.Set as S
import qualified Data.Map as M

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
type Term = Term' Name Natural

instance Show a => Show (Universe' a) where
  show (UVar a) = show a
  show (UMax a b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (UAdd a b) = show a ++ " + " ++ show b
  show (ULit n) = show n

instance (Integral a, Show a, Show u) => Show (Term' u a) where
  show = showTerm

showExpr :: (Integral a, Show u, Show a) => Bool -> [Name] -> Term' u a -> String
showExpr isType l = \case
  Var a     -> if a >= len l then "?" ++ show a else show $ genericIndex l a
  App f e   -> goLamPar f ++ " (" ++ go e ++ ")"
  Lam a t e ->
    let (p, q) = (isType, occursFree 0 e) in
    if | p && q     -> "∀ " ++ show a ++ " : " ++ go t ++ ", " ++ goNest a e
       | p && not q -> goVarPar t ++ " -> " ++ goNest a e
       | otherwise  -> "λ " ++ show a ++ " : " ++ showExpr True l t ++ ". " ++ goNest a e
  Type u -> "Type " ++ show u
  where
    go = showExpr isType l

    goLamPar f@(Lam _ _ _) = "(" ++ go f ++ ")"
    goLamPar f = go f

    goVarPar v@(Var _) = go v
    goVarPar v = "(" ++ go v ++ ")"

    goNest a e = showExpr isType (a : l) e

showTerm :: (Integral b, Show a, Show b) => Term' a b -> String
showTerm = showExpr False []

showType :: (Integral b, Show a, Show b) => Term' a b -> String
showType = showExpr True []

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

step' :: Integral a => Term' u a -> Writer Any (Term' u a)
step' (Var a)              = return $ Var a
step' (App (Lam _ _ e) e') = return $ substitute e' e
step' (App f e)            = tell (Any True) *> (flip App e <$> step' f)
step' (Lam a t e)          = Lam a <$> step' t <*> step' e
step' (Type u)             = return $ Type u

step :: Integral a => Term' u a -> Term' u a
step e = fst . runWriter $ step' e

execute :: Integral a => Term' u a -> [Term' u a]
execute e =
  case runWriter (step' e) of
    (e', Any True) -> e : execute e'
    (e', _)        -> [e, e']

evaluate :: Integral a => Term' u a -> Term' u a
evaluate = last . execute

type Constraint = (Universe, Universe)
type Env = [(Name, Term)]
type CheckM = ExceptT String (StateT (Env, [Constraint]) IO)

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines

showConstraints :: [Constraint] -> String
showConstraints = intercalate "\n" . map (leq . bimap show show) where
  leq (l, r) = l ++ " ≤ " ++ r

subtype :: Term -> Term -> CheckM Bool
subtype (Var a) (Var a') = return $ a == a'
subtype (App f e) (App f' e') = (&&) <$> subtype f f' <*> subtype e e'
subtype (Lam _ t e) (Lam _ t' e') = (&&) <$> subtype t t' <*> subtype e e'
subtype (Type u) (Type u') = do
  modify (second ((u, u') :))
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
      "Can't match '" ++ showExpr True (fst <$> env) s ++
      "' with '" ++ showExpr True (fst <$> env) t ++ "'"

(!?) :: (Show a, Integral a) => [Term' u a] -> a -> CheckM (Term' u a)
l !? k
  | k < len l = return $ genericIndex l k ↑ (k + 1)
  | otherwise = throwError $ "Variable not in scope: " ++ show k

infer' :: Term -> CheckM Term
infer' (Var a) = (!? a) =<< map snd . fst <$> get
infer' (App f e) = do
  tf <- infer' f
  case tf of
    Lam _ t e' -> (infer' e >>= (`checkSubtype` t)) $> substitute e e'
    _ -> throwError $ "Non-functional construction: " ++ show f ++ " : " ++ show tf
infer' (Lam a t e) =
  Lam a t <$> (infer' t *> modify (first ((a, t) :)) *> infer' e <* modify (first tail))
infer' (Type u) = return $ Type (UAdd u (ULit 1))

infer :: Term -> IO (Either String Term)
infer = flip evalStateT ([], []) . runExceptT . infer'

normalize' :: Eq a => Universe' a -> Universe' a
normalize' = \case
  UVar a            -> UVar a
  ULit a            -> ULit a
  UMax (UMax a b) c -> UMax (go a) (UMax (go b) (go c))
  UMax a b          -> UMax (go a) (go b)
  UAdd a (UMax b c) -> UMax (UAdd (go a) (go b)) (UAdd (go a) (go c))
  UAdd (UMax a b) c -> UMax (UAdd (go a) (go c)) (UAdd (go b) (go c))
  UAdd a b          -> UAdd (go a) (go b)
  where go = normalize'

normalize :: Eq a => Universe' a -> Universe' a
normalize e =
  let e' = normalize' e in
  if e' == e then e' else normalize e'

toSym :: [Constraint] -> Symbolic SBool
toSym constraints = do
  vars' <- sequence vars
  let nats = (literal 0 .<=) . snd <$> M.toList vars'
  let clauses = uncurry ((.<=) `on` sym vars') <$> constraints
  return $ bAnd (nats ++ clauses)
  where
    names = S.toList . foldMap (foldMap S.singleton . uncurry UAdd) $ constraints
    vars = M.fromList $ zip names (exists . unName <$> names)
    sym _     (ULit n) = literal (fromIntegral n :: Integer)
    sym vars' (UVar s) = vars' ! s
    sym vars' (UMax a b) = smax (sym vars' a) (sym vars' b)
    sym vars' (UAdd a b) = sym vars' a + sym vars' b
