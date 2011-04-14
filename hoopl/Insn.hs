{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Insn (Insn(..),Expr(..)) where
import Compiler.Hoopl
-- a side effect free expression
-- FIXME handle Box and Ann smartly
data Expr = Double Double |  StrLit String | ScopedLex Expr | Reg Int
    deriving (Show,Eq)

data Insn e x where
    Fetch :: Int -> Expr -> Insn O O
    Subcall :: Int -> [Expr] -> Insn O O
    BifPlus :: Int -> Expr -> Expr -> Insn O O
    RegSet  :: Int -> Expr -> Insn O O
