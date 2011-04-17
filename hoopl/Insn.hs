{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Insn (Insn(..),Expr(..),mapE,insnToGraph,insnTarget) where
import Compiler.Hoopl
-- a side effect free expression
-- FIXME handle Box and Ann smartly
data Expr = Double Double |  StrLit String | ScopedLex Expr | Reg Int
    deriving (Show,Eq)

data Insn e x where
    Fetch :: Int -> Expr -> Insn O O
    Subcall :: Int -> [Expr] -> Insn O O
    BifPlus :: Int -> Expr -> Expr -> Insn O O
    BifMinus :: Int -> Expr -> Expr -> Insn O O
    BifDivide :: Int -> Expr -> Expr -> Insn O O
    RegSet  :: Int -> Expr -> Insn O O

mapE :: (Expr -> Expr) -> Insn e x -> Insn e x
mapE func (BifPlus reg a b) = BifPlus reg (func a) (func b)
mapE func (BifDivide reg a b) = BifDivide reg (func a) (func b)
mapE func (BifMinus reg a b) = BifMinus reg (func a) (func b)
mapE func (Subcall reg args)   = Subcall reg (map func args)
mapE func (RegSet reg a) = RegSet reg (func a) 
mapE func (Fetch reg a) = Fetch reg (func a)

insnToGraph :: Insn e x -> Graph Insn e x
insnToGraph n@(Fetch _ _)   = mkMiddle n
insnToGraph n@(Subcall _ _)    = mkMiddle n
insnToGraph n@(BifPlus   _ _ _)    = mkMiddle n
insnToGraph n@(BifDivide _ _ _)    = mkMiddle n
insnToGraph n@(BifMinus _ _ _)    = mkMiddle n
insnToGraph n@(RegSet _ _)    = mkMiddle n


insnTarget :: Insn e x -> Maybe Int
insnTarget insn = Just $ r insn
    where 
          r :: Insn e x -> Int
          r (Fetch reg _) = reg
          r (Subcall reg _) = reg
          r (BifPlus reg _ _) = reg
          r (BifMinus reg _ _) = reg
          r (BifDivide reg _ _) = reg
          r (RegSet reg _) = reg
