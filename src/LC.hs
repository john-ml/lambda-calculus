{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module LC
  ( Term (..)
  , (-->)
  , mapExpr
  , mapVars
  , mapHoles
  , pretty
  , typeWithNames
  , termWithNames
  , substitute
  , fill
  , reduce
  , sizeof
  , simplify
  , evaluate
  , check
  , infer
  ) where

import Data.Map
import Control.Monad.Except
import Data.List (intercalate)
import Control.Monad.Writer

data Term
  = Var Int
  | Hole String
  | Term :$ Term
  | Term :--> Term
  | Type Int
  deriving (Eq, Ord)
infixl 7 :$
infixr 5 :-->

type Type = Term
type Env = [Type]

(-->) :: Type -> Type -> Type
a --> b = a :--> adjustFree (+ 1) b
infixr 6 -->

occursFree :: Int -> Term -> Bool
occursFree n (Var a) = a == n
occursFree _ (Hole _) = False
occursFree n (f :$ e) = occursFree n f || occursFree n e
occursFree n (t :--> e) = occursFree n t || occursFree (n + 1) e
occursFree _ (Type _) = False

instance Show Term where
  show (Var a)  = show a
  show (f :$ e) = "(" ++ show f ++ ") (" ++ show e ++ ")"
  show (t :--> e)
    | occursFree 0 e = "λ (" ++ show t ++ ") " ++ show e ++ ""
    | otherwise      = "(" ++ show t ++ ") -> " ++ show e ++ ""
  show (Hole s) = s
  show (Type n) = "Type" ++ show n

mapExpr
  :: (Env -> Int -> Term)
  -> (Env -> String -> Term)
  -> Term
  -> Term
mapExpr f g = go [] where
  go l (Var a)   = f l a
  go l (Hole s)  = g l s
  go l (f' :$ e) = go l f' :$ go l e
  go l (t :--> e) = go l t :--> go (t : l) e
  go _ (Type n)  = Type n

mapVars :: (Env -> Int -> Term) -> Term -> Term
mapVars f = mapExpr f (const Hole)

mapHoles :: (Env -> String -> Term) -> Term -> Term
mapHoles g = mapExpr (const Var) g

pretty :: Map String Term -> Term -> String
pretty m = show . go [] where
  m' = fromList $ (\ (x, y) -> (y, x)) <$> toList m
  try e handler = maybe handler Hole (m' !? e)
  go l e@(f :$ e') = try e (go l f :$ go l e')
  go l e@(t :--> e') = try e (go l t :--> go (t : l) e')
  go _ e = e

names :: [String]
names = "" : (pure <$> letters) ++ (cat <$> [1..] <*> letters) where
  letters = ['a' .. 'z']
  cat n l = l : show n

withNames :: Bool -> Term -> String
withNames = flip go 0 where
  go _ n (Var k) = names !! (n - k)
  go _ _ (Hole s) = "[" ++ s ++ "]"
  go p n (f :$ e) = go p n f ++ "(" ++ go p n e ++ ")"
  go p n (f :--> e) = lhs p (occursFree 0 e) ++ go p (n + 1) e where
    lhs True True   = "λ (" ++ (names !! (n + 1)) ++ " : " ++ go False n f ++ "). "
    lhs True False  = "λ (_ : " ++ go False n f ++ "). " 
    lhs False True  = "∀ (" ++ (names !! (n + 1)) ++ " : " ++ go False n f ++ "), "
    lhs False False = "(" ++ go False n f ++ ") -> "
  go _ _ (Type n) = "Type" ++ show n

typeWithNames :: Term -> String
typeWithNames = withNames False

termWithNames :: Term -> String
termWithNames = withNames True

adjustFree :: (Int -> Int) -> Term -> Term
adjustFree f = mapVars (\ l a ->
  let n = fromIntegral $ length l in
  if a >= n then Var (f a) else Var a)

substitute :: Term -> Term -> Term
substitute e = mapVars (\ l a ->
  let n = fromIntegral $ length l in
  if | a > n     -> Var (a - 1)
     | a == n    -> adjustFree (+ n) e
     | otherwise -> Var a)

fill :: Map String Term -> Term -> Term
fill m = mapHoles (\ l s ->
  let n = fromIntegral $ length l in
  case m !? s of
    Just e  -> adjustFree (+ n) e
    Nothing -> Hole s)

reduce :: Term -> Term
reduce (Var a)       = Var a
reduce ((_ :--> e') :$ e) = substitute e e'
reduce (Var n :$ e)  = Var n :$ reduce e
reduce (f :$ e)      = reduce f :$ e
reduce (t :--> e)     = reduce t :--> reduce e
reduce (Hole s)      = Hole s
reduce (Type n)      = Type n

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
sizeof (t :--> e)  = 1 + sizeof t + sizeof e
sizeof (Type _) = 1

whileNot :: (a -> a -> Bool) -> (a -> a) -> a -> a
whileNot p f a =
  let a' = f a in
  if p a' a then a else whileNot p f a'

isSubtype :: Type -> Type -> Bool
infix 4 `isSubtype`
Var a `isSubtype` Var b = a == b
Hole a`isSubtype`  Hole b = a == b
f :$ e `isSubtype` f' :$ e' = f `isSubtype` f' && e `isSubtype` e'
t :--> e `isSubtype` t' :--> e' = t `isSubtype` t' && e `isSubtype` e'
Type n `isSubtype` Type m = n <= m
_ `isSubtype` _ = False

type CheckM = ExceptT String (Writer String)

prettyEnv :: Env -> String
prettyEnv = intercalate "\n" . zipWith prettyEntry [0..] where
  prettyEntry n t' = show n ++ " : " ++ show t'

whisper :: String -> CheckM ()
whisper = const $ return () --tell . (++ "\n")

showHole :: Env -> String -> Maybe Type -> String
showHole env s t =
  unlines
    [ "Found hole: " ++ s ++ (case t of Just t' -> " : " ++ show t'; Nothing -> "")
    , "Context:"
    ] ++
  prettyEnv env

check' :: Env -> Term -> (Type -> Type -> Bool) -> Type -> CheckM ()
check' env (Hole s) _ t = throwError $ showHole env s (Just t)
check' env e r t = do
  inferred <- infer' env e
  whisper $
    show inferred ++ " ~ " ++ show t ++ " = " ++
    show (inferred `r` t)
  if inferred `r` t then
     return ()
  else
     throwError $
       unlines
         [ "Expected '" ++ show t ++ "', actual '" ++ show inferred ++ "' in:"
         , show e
         , "Context:"
         ] ++
       prettyEnv env

get :: Env -> Int -> CheckM Term
get = go 0 where
  go _ [] _ = throwError $ "get failed"
  go k (h : t) n
    | k == n = return $ adjustFree (+ (k + 1)) h
    | otherwise = go (k + 1) t n

infer' :: Env -> Term -> CheckM Type
infer' env (Var a) = get env a
infer' env (Hole s) = throwError $ showHole env s Nothing
infer' env (f :$ e) = do
  tf <- infer' env f
  whisper $ "infer' env (" ++ show f ++ ") = " ++ show tf
  whisper $ "Context:"
  whisper $ prettyEnv env
  case tf of
    te :--> tret -> do
      check' env e isSubtype te
      whisper $ "check' env (" ++ show e ++ ") (" ++ show te ++ ") OK\n"
      return (substitute e tret)
    _ -> throwError $ "Non-functional construction: " ++ show f ++ " : " ++ show tf
infer' env (t :--> e) = do
  _ <- infer' env t
  (t :-->) <$> infer' (t : env) e
infer' _ (Type n) = return $ Type (n + 1)
  
check :: Term -> Type -> CheckM ()
check e t = check' [] e (flip isSubtype) t

infer :: Term -> CheckM Type
infer = infer' []
