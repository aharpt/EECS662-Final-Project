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
eval e (Lambda i t b) = Nothing --TODO:finish this function

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

-- Exercise 1: Implementing type ---
typeof :: TERMLANG -> (Maybe TYPELANG)
typeof _ = Nothing
typeof (Num n) = if n<0 
					then Nothing 
					else return TNum
typeof (Plus l r) = do {TNum <- typeof l;
						TNum <- typeof r;
						return TNum}
typeof (Minus l r) = do {TNum <- typeof l;
						 TNum <- typeof r;
						 return TNum}						 
typeof (Mult l r) = do {TNum <- typeof l;
						TNum <- typeof r;
						return TNum}
typeof (Div l r) = do {TNum <- typeof l;
					   TNum <- typeof r;
					   return TNum}
typeof (Boolean b) = return TBool
typeof (And l r) = do {TBool <- typeof l;
					   TBool <- typeof r;
					   return TBool}						 
typeof (Or l r) = do {TBool <- typeof l;
					  TBool <- typeof r;
					  return TBool}
typeof (IsZero x) = do {TNum <- typeof x;
						return TBool	}
typeof (Leq l r) = do {TNum <- typeof l;
					   TNum <- typeof r;
					   return TBool}		
typeof (If c t e) = do {TBool <- typeof c;
                        t' <- typeof t;
                        e' <- typeof e;
                        if t'==e' then return t' else Nothing}
typeof (Bind i v b) = do {tv <- typeof v;
					      typeof ((i,tv):g) b}
typeof (Id i) = (lookup i g)