{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions
data TYPELANG = TNum
              | TBool
                deriving (Show,Eq)

data TERMLANG = Num  Int 
	          | Boolean Bool -- True False
              | Id String 
              | Plus TERMLANG TERMLANG 
              | Minus TERMLANG TERMLANG
              | Mult TERMLANG TERMLANG
              | Div TERMLANG TERMLANG
	          | Lambda String TERMLANG TERMLANG
	          | App TERMLANG TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | If TERMLANG TERMLANG TERMLANG
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG 
               deriving (Show,Eq)


--type Env = [(String,TERMLANG)]
--type Cont = [(String,TYPELANG)]

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
					      typeofM ((i,tv):g) b}
typeof (Id i) = (lookup i g)


-- Exercise 2: 