{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions
data TERMLANG = Boolean Bool
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG 
              | If TERMLANG TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | Id String 
                deriving (Show,Eq)
