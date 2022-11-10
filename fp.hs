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
	          | Lambda String TERMLANG TERMLANG-- may need to be udpated
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

-- Exercise 2: 