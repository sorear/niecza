{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Insn (Reg,Insn(..),Expr(..),mapE,insnToGraph,insnTarget,exprs) where
import Compiler.Hoopl
-- a side effect free expression
-- FIXME handle Box and Ann smartly

type Reg = Int
data Expr = Double Double |  StrLit String | ScopedLex String | Reg Reg
    deriving (Show,Eq)

data Insn e x where
    Fetch :: Reg -> Expr -> Insn O O
    Subcall :: Reg -> [Expr] -> Insn O O
    BifPlus :: Reg -> Expr -> Expr -> Insn O O
    BifMinus :: Reg -> Expr -> Expr -> Insn O O
    BifDivide :: Reg -> Expr -> Expr -> Insn O O
    RegSet  :: Reg -> Expr -> Insn O O

deriving instance Show (Insn e x)

-- map over all the expressions inside
mapE :: (Expr -> Expr) -> Insn e x -> Insn e x
mapE func (BifPlus reg a b) = BifPlus reg (func a) (func b)
mapE func (BifDivide reg a b) = BifDivide reg (func a) (func b)
mapE func (BifMinus reg a b) = BifMinus reg (func a) (func b)
mapE func (Subcall reg args)   = Subcall reg (map func args)
mapE func (RegSet reg a) = RegSet reg (func a) 
mapE func (Fetch reg a) = Fetch reg (func a)

-- expresions in the instruction
exprs :: Insn e x -> [Expr]
exprs (BifPlus reg a b) = [a,b]
exprs (BifDivide reg a b) = [a,b]
exprs (BifMinus reg a b) = [a,b]
exprs (Subcall reg args) = args
exprs (RegSet reg a) = [a] 
exprs (Fetch reg a) = [a]


-- convert an expression to a graph containing only it
insnToGraph :: Insn e x -> Graph Insn e x
insnToGraph n@(Fetch _ _)   = mkMiddle n
insnToGraph n@(Subcall _ _)    = mkMiddle n
insnToGraph n@(BifPlus   _ _ _)    = mkMiddle n
insnToGraph n@(BifDivide _ _ _)    = mkMiddle n
insnToGraph n@(BifMinus _ _ _)    = mkMiddle n
insnToGraph n@(RegSet _ _)    = mkMiddle n


-- the register the instruction writes to
insnTarget :: Insn e x -> Maybe Reg
insnTarget insn = Just $ r insn
    where 
          r :: Insn e x -> Reg
          r (Fetch reg _) = reg
          r (Subcall reg _) = reg
          r (BifPlus reg _ _) = reg
          r (BifMinus reg _ _) = reg
          r (BifDivide reg _ _) = reg
          r (RegSet reg _) = reg
