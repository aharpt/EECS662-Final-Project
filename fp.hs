{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- Definitions for types
data TYPELANG = TNum
              | TBool
              | TYPELANG :->: TYPELANG --type for functions
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
                deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  BooleanV :: Bool -> VALUELANG
  ClosureV :: String -> TERMLANG -> Env -> VALUELANG
  deriving (Show,Eq)

-- Environment for eval
type Env = [(String,VALUELANG)]
-- Environment for typeof
type Cont = [(String,TYPELANG)]

--Part 2 - Evaluation
eval :: Env -> TERMLANG -> (Maybe VALUELANG)
eval e (Num x) = if x < 0 then Nothing else return (NumV x)
eval e (Plus l r) = do {
  (NumV l') <- eval e l;
  (NumV r') <- eval e r;
  return (NumV (l' + r'))
}
eval e (Minus l r) = do {
  (NumV l') <- eval e l;
  (NumV r') <- eval e r;
  if (l' - r') < 0 then Nothing else return (NumV (l' - r'))
}
eval e (Mult l r) = do {
  (NumV l') <- eval e l;
  (NumV r') <- eval e r;
  return (NumV (l' * r'))
}
eval e (Div l r) = do {
  (NumV l') <- eval e l;
  (NumV r') <- eval e r;
  if r' == 0 then Nothing else return (NumV (l' `div` r'))
}
eval e (Lambda i t b) = return (ClosureV i b e) -- t is the type of the domain, not used in evaluation
eval e (Boolean x) = return (BooleanV x)
eval e (And l r) = do {
                      (BooleanV l') <- eval e l;
                      (BooleanV r') <- eval e r;
                      Just (BooleanV (l' && r'))
                    }
eval e (Or l r) = do {
                      (BooleanV l') <- eval e l;
                      (BooleanV r') <- eval e r;
                      Just (BooleanV (l' || r'))
                    }
eval e (IsZero x) = do {
  (NumV x') <- eval e x;
  if (x' == 0) then Just (BooleanV True) else Just (BooleanV False)
}
eval e (Leq l r) = do {
  (NumV l') <- eval e l;
  (NumV r') <- eval e r;
  Just (BooleanV (l' <= r'))
}
eval e (If x y z) = do {
  (BooleanV x') <- eval e x;
  if (x') then (eval e y) else (eval e z)
}

eval e (Bind i v b) = do {
    v' <- eval e v;
    eval ((i,v'):e) b
}
eval e (Id i) = (lookup i e)
eval e (App f a) = do {
            (ClosureV i b j) <- eval e f;
            v <- eval e a;
            eval ((i,v):j) b
          }


-- Part 1 - Type Inference
-- typeof [("x", TNum)] (Num 3)
typeof :: Cont -> TERMLANG -> (Maybe TYPELANG)
typeof g (Num n) = if n<0
					then Nothing
					else return TNum
typeof g (Boolean b) = return TBool
typeof g (And l r) = do { TBool <- typeof g l;
		 				  TBool <- typeof g r;
		 				  return TBool}
typeof g (Or l r) = do { TBool <- typeof g l;
		 				 TBool <- typeof g r;
		 				 return TBool}
typeof g (Leq l r) = do { TNum <- typeof g l;
		 				  TNum <- typeof g r;
		 				  return TBool}
typeof g (IsZero x) = do {TNum <- typeof g x;
		 				   return TBool	}
typeof g (If c t e) = do {TBool <- typeof g c;
                           t' <- typeof g t;
                           e' <- typeof g e;
                           if t'==e' then return t' else Nothing}
typeof g (Lambda i t b) = return (t:->:t)
typeof g (App f a) = do { d :->: r <- typeof g f;
                          a' <- typeof g a;
                          if d == a' then return a'
                          else Nothing}
                          
typeof g (Bind i v b) = do {v' <- typeof g v;
                            typeof ((i,v'):g) b}
typeof g (Id i) = lookup i g
typeof g (Plus l r) = do { TNum <- typeof g l;
                          TNum <- typeof g r;
                          return TNum}
typeof g (Minus l r) = do { TNum <- typeof g l;
                          TNum <- typeof g r;
                          return TNum}
typeof g (Mult l r) = do { TNum <- typeof g l;
                          TNum <- typeof g r;
                          return TNum}
typeof g (Div l r) = do { TNum <- typeof g l;
                          TNum <- typeof g r;
                          return TNum}