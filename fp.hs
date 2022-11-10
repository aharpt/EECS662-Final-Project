{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- Definitions for types
data TYPELANG = TNum
        | TBool
        | TYPELANG :->: TYPELANG
          deriving(Show,Eq)

-- AST and Type Definitions
data TERMLANG = Boolean Bool
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG 
              | If TERMLANG TERMLANG TERMLANG
              | Lambda String TYPELANG TERMLANG
              | App TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | Id String 
              | Num Int
              | Plus TERMLANG TERMLANG
              | Minus TERMLANG TERMLANG
              | Mult TERMLANG TERMLANG
              | Div TERMLANG TERMLANG
                deriving (Show,Eq)

-- Environment for eval
type Env = [(String,TERMLANG)]

eval :: Env -> TERMLANG -> (Maybe TERMLANG)
eval e (Num x) = if x < 0 then Nothing else return (Num x)
eval e (Plus l r) = do {
  (Num l') <- eval e l;
  (Num r') <- eval e r;
  return (Num (l' + r'))
}
eval e (Minus l r) = do {
  (Num l') <- eval e l;
  (Num r') <- eval e r;
  if (l' - r') < 0 then Nothing else return (Num (l' + r'))
}
eval e (Mult l r) = do {
  (Num l') <- eval e l;
  (Num r') <- eval e r;
  return (Num (l' * r'))
}
eval e (Div l r) = do {
  (Num l') <- eval e l;
  (Num r') <- eval e r;
  if r' == 0 then Nothing else return (Num (l' `div` r'))
}
eval e (Lambda i t b) = Nothing --TODO:finish this function
eval e _ = Nothing