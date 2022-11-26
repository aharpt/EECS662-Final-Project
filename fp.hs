{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad
import Text.Show.Functions

-- Definitions for types
data TYPELANG = TNum
              | TBool
              | TYPELANG :->: TYPELANG --type for functions
              | TLoc
              | TTop
                deriving (Show,Eq)

-- AST and Type Definitions
data TERMLANG = Num  Int
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

data VALUELANG where
  NumV :: Int -> VALUELANG
  BooleanV :: Bool -> VALUELANG
  ClosureV :: String -> TERMLANG -> Env -> VALUELANG
  LocV :: Int -> VALUELANG
  TopV :: VALUELANG -> VALUELANG
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
  (TopV t) <- lookup i e;
  return (store, t)
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
eval e store (Seq l r) = do{
  (store', _) <- eval e store l;
  eval e store' r
}
eval e store (Fix f) = do { 
	(store', ClosureV i b e) <- (eval e store f);
   	 Nothing -- eval e (subst i (Fix (Lambda i b)) b)
}
eval e store _ = Nothing


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
  TLoc <- typeof g t;
  return TLoc
}
typeof g (Set l v) = do {
  TLoc <- typeof g l;
  typeof g v
}
typeof g (Seq l r) = do {
  typeof g r;
}
typeof g (Fix f) = do { 
  (d :->: r) <- typeof g f ;
  return r 
}
typeof g _ = Nothing