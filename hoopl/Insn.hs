{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Insn (Insn(..),Expr(..),mapE,insnToGraph) where
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

mapE :: (Expr -> Expr) -> Insn e x -> Insn e x
mapE func (BifPlus reg a b) = BifPlus reg (func a) (func b)
mapE func (Subcall reg args)   = Subcall reg (map func args)
mapE func (RegSet reg a) = RegSet reg (func a) 
mapE func (Fetch reg a) = Fetch reg (func a)

insnToGraph :: Insn e x -> Graph Insn e x
insnToGraph n@(Fetch _ _)   = mkMiddle n
insnToGraph n@(Subcall _ _)    = mkMiddle n
insnToGraph n@(BifPlus _ _ _)    = mkMiddle n
insnToGraph n@(RegSet _ _)    = mkMiddle n
