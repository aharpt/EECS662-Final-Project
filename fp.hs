{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad
import Text.Show.Functions
import Data.Maybe

-- Definitions for types
data TYPELANG = TNum
              | TBool
              | TYPELANG :->: TYPELANG --type for functions
              | TLoc
              | TTop
                deriving (Show,Eq)

-- AST and Type Definitions
data TERMLANG = Num Int
              | Boolean Bool -- True False
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG
              | If TERMLANG TERMLANG TERMLANG
              | Lambda String TYPELANG TERMLANG
              | App TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | Id String
              | Plus TERMLANG TERMLANG
              | Minus TERMLANG TERMLANG
              | Mult TERMLANG TERMLANG
              | Div TERMLANG TERMLANG
              | New TERMLANG
              | Set TERMLANG TERMLANG
              | Deref TERMLANG
              | Seq TERMLANG TERMLANG
              | Fix TERMLANG
                deriving (Show,Eq)

-- AST and Type Definitions
data TERMLANGX = NumX Int
              | BooleanX Bool -- True False
              | AndX TERMLANGX TERMLANGX
              | OrX TERMLANGX TERMLANGX
              | LeqX TERMLANGX TERMLANGX
              | IsZeroX TERMLANGX
              | IfX TERMLANGX TERMLANGX TERMLANGX
              | LambdaX String TYPELANG TERMLANGX
              | AppX TERMLANGX TERMLANGX
              | BindX String TERMLANGX TERMLANGX
              | IdX String
              | PlusX TERMLANGX TERMLANGX
              | MinusX TERMLANGX TERMLANGX
              | MultX TERMLANGX TERMLANGX
              | DivX TERMLANGX TERMLANGX
              | NewX TERMLANGX
              | SetX TERMLANGX TERMLANGX
              | DerefX TERMLANGX
              | SeqX TERMLANGX TERMLANGX
              | FixX TERMLANGX
              | AssignX TERMLANGX TERMLANGX
                deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  BooleanV :: Bool -> VALUELANG
  ClosureV :: String -> TERMLANG -> Env -> VALUELANG
  LocV :: Int -> VALUELANG
  deriving (Show,Eq)


-- Environment for eval
type Env = [(String,VALUELANG)]
-- Environment for typeof
type Cont = [(String,TYPELANG)]

-- types for store
type Loc = Int
type StoreFunc = Loc -> Maybe VALUELANG
type Store = (Loc, StoreFunc)

-- Initialize the store
initialize :: StoreFunc
initialize x = Nothing

initializeStore :: Store
initializeStore = (0, initialize)

-- needed for newStore
set :: StoreFunc -> Loc -> VALUELANG -> StoreFunc
set s l v = \m -> if m==l then (Just v) else (s)(m)

-- function to help with new function in eval
newStore :: Store -> VALUELANG -> Store
newStore (i,s) v = ((i+1), set s i v)

-- setStore function
setStore :: Store -> Loc -> VALUELANG -> Store
setStore (i,s) l v = (i,(set s l v))

-- deref function
deref :: StoreFunc -> Loc -> Maybe VALUELANG
deref s l = (s)(l)

derefStore :: Store -> Int -> Maybe VALUELANG
derefStore (i,s) l = deref s l

-- subst function for fix
subst :: String -> TERMLANG -> TERMLANG -> TERMLANG
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = IsZero (subst i v x)
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Id i') = if i==i' then v else (Id i')
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Fix f) = (Fix (subst i v f))
subst i v (Lambda i' ty b) = if i==i'
                             then (Lambda i ty (subst i v b))
                             else (Lambda i' ty (subst i b b))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (New t) = (New (subst i v t))
subst i v (Deref t) = (Deref (subst i v t))
subst i v (Set l v') = (Set (subst i v l) (subst i v v'))
subst i v (Seq l r) = (Seq (subst i v l) (subst i v r))
-- subst i v _ = (Num 5) -- need implement: new, deref, set, seq

--Part 2 - Evaluation
eval :: Env -> Store -> TERMLANG -> Maybe (Store, VALUELANG)
eval e store (Num x) = if x < 0 then Nothing else return (store, (NumV x))
eval e store (Plus l r) = do {
  (store', (NumV l')) <- eval e store l;
  (store'', (NumV r')) <- eval e store' r;
  return (store'', (NumV (l' + r')))
}
eval e store (Minus l r) = do {
  (store', (NumV l')) <- eval e store l;
  (store'', (NumV r')) <- eval e store' r;
  if (l' - r') < 0 then Nothing else return (store'', (NumV (l' - r')))
}
eval e store (Mult l r) = do {
  (store', (NumV l')) <- eval e store l;
  (store'', (NumV r')) <- eval e store' r;
  return (store'', (NumV (l' * r')))
}
eval e store (Div l r) = do {
  (store', (NumV l')) <- eval e store l;
  (store'', (NumV r')) <- eval e store' r;
  if r' == 0 then Nothing else return (store'', (NumV (l' `div` r')))
}
eval e store (Lambda i t b) = return (store, (ClosureV i b e)) -- t is the type of the domain, not used in evaluation
eval e store (Boolean x) = return (store, (BooleanV x))
eval e store (And l r) = do {
  (store', (BooleanV l')) <- eval e store l;
  (store'', (BooleanV r')) <- eval e store' r;
  return (store'', (BooleanV (l' && r')))
}
eval e store (Or l r) = do {
  (store', (BooleanV l')) <- eval e store l;
  (store'', (BooleanV r')) <- eval e store' r;
  return (store'', (BooleanV (l' || r')))
}
eval e store (IsZero x) = do {
  (store', (NumV x')) <- eval e store x;
  if (x' == 0) then return (store', (BooleanV True)) else return (store', (BooleanV False))
}
eval e store (Leq l r) = do {
  (store', (NumV l')) <- eval e store l;
  (store'', (NumV r')) <- eval e store' r;
  return (store'', (BooleanV (l' <= r')))
}
eval e store (If x y z) = do {
  (store', (BooleanV x')) <- eval e store x;
  if (x') then (eval e store' y) else (eval e store' z)
}
eval e store (Bind i v b) = do {
    (store', v') <- eval e store v;
    eval ((i,v'):e) store' b
}
eval e store (Id i) = do {
  if (lookup i e == Nothing) then Nothing else Just (store, fromJust (lookup i e))
}
eval e store (App f a) = do {
   (store', (ClosureV i b j)) <- eval e store f;
   (store'', v) <- eval e store' a;
   eval ((i,v):j) store'' b
}
eval e store (New t) = do{
  ((l, tstore), v) <- eval e store t;
  return ((newStore (l, tstore) v), (LocV l))
}
eval e store (Set l v) = do{
   (store', (LocV l')) <- eval e store l;
   (store'', v') <- eval e store' v;
   return ((setStore store'' l' v'), v')
}
eval e store (Deref l) = do {
   (store', LocV l') <- eval e store l;
   v <- derefStore store' l';
   return (store', v)
}
eval e store (Seq l r) = do{
  (store', _) <- eval e store l;
  eval e store' r
}
eval e store (Fix f) = do {
  (store', ClosureV i b e) <- (eval e store f);
   eval e store (subst i (Fix (Lambda i TNum b)) b)
}



-- Part 1 - Type Inference
-- typeof [("x", TNum)] (Num 3)
typeof :: Cont -> TERMLANG -> (Maybe TYPELANG)
typeof g (Num n) = if n<0 then Nothing else return TNum
typeof g (Boolean b) = return TBool
typeof g (And l r) = do {
    TBool <- typeof g l;
    TBool <- typeof g r;
    return TBool
}
typeof g (Or l r) = do {
   TBool <- typeof g l;
   TBool <- typeof g r;
   return TBool
}
typeof g (Leq l r) = do {
   TNum <- typeof g l;
   TNum <- typeof g r;
   return TBool
}
typeof g (IsZero x) = do {
   TNum <- typeof g x;
   return TBool
}
typeof g (If c t e) = do {
   TBool <- typeof g c;
   t' <- typeof g t;
   e' <- typeof g e;
   if t'==e' then return t' else Nothing
}
typeof g (Lambda i d b) = do {
    r <- (typeof ((i, d):g) b);
    return (d:->:r)
}
typeof g (App f a) = do {
   d :->: r <- typeof g f;
   a' <- typeof g a;
   if d == a' then return a'
   else Nothing
}
typeof g (Bind i v b) = do {
    v' <- typeof g v;
    typeof ((i,v'):g) b
}
typeof g (Id i) = lookup i g
typeof g (Plus l r) = do {
    TNum <- typeof g l;
    TNum <- typeof g r;
    return TNum
}
typeof g (Minus l r) = do {
    TNum <- typeof g l;
    TNum <- typeof g r;
    return TNum
}
typeof g (Mult l r) = do {
    TNum <- typeof g l;
    TNum <- typeof g r;
    return TNum
}
typeof g (Div l r) = do {
    TNum <- typeof g l;
    TNum <- typeof g r;
    return TNum
}
typeof g (New t) = do{
  typeof g t;
  return TLoc
}
typeof g (Set l v) = do {
  TLoc <- typeof g l;
  typeof g v
}
typeof g (Deref t) = do { 
  TLoc <- (typeof g t);
  return TTop
}
typeof g (Seq l r) = do {
  typeof g r;
}
typeof g (Fix f) = do {
  (d :->: r) <- typeof g f ;
  return r
}

-- Elaborator
elab :: TERMLANGX -> TERMLANG
elab (NumX x) = (Num x)
elab (PlusX l r) = (Plus (elab l) (elab r))
elab (MinusX l r) = (Minus (elab l) (elab r))
elab (MultX l r) = (Mult (elab l) (elab r))
elab (DivX l r) = (Div (elab l) (elab r))
elab (LambdaX i t b) = (Lambda i t (elab b))
elab (BooleanX x) = (Boolean x)
elab (AndX l r) = (And (elab l) (elab r))
elab (OrX l r) = (Or (elab l) (elab r))
elab (IsZeroX x) = (IsZero (elab x))
elab (LeqX l r) = (Leq (elab l) (elab r))
elab (IfX x y z) = (If (elab x) (elab y) (elab z))
elab (BindX i v b) = (Bind i (elab v) (elab b))
elab (IdX i) = (Id i)
elab (AppX f a) = (App (elab f) (elab a))
elab (NewX t) = (New (elab t))
elab (SetX l v) = (Set (elab l) (elab v))
elab (DerefX l) = (Deref (elab l))
elab (SeqX l r) = (Seq (elab l) (elab r))
elab (FixX f) = (Fix (elab f))
elab (AssignX l r) = (Set (elab l) (elab r))

-- Interpretor
interp :: TERMLANGX -> Maybe (Store, VALUELANG)
interp x = if (typeof [] (elab x) == Nothing) then Nothing else (eval [] initializeStore (elab x))