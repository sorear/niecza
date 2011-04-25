{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Insn (Reg,Insn(..),Expr(..),Op(..),mapE,insnToGraph,exprs) where
import Compiler.Hoopl
-- a side effect free expression
-- FIXME handle Box and Ann smartly

type Reg = Int
data Expr = Double Double |  StrLit String | ScopedLex String | CoreLex String | Reg Reg
    deriving (Show,Eq)

data Insn e x where
    CondBranch  :: Expr -> Label -> Label -> Insn O C
    Goto :: Label -> Insn O C
    Label :: Label -> Insn C O
    Op :: Reg -> Op  -> Insn O O

data Op where
    Fetch :: Expr -> Op
    Subcall :: [Expr] -> Op
    BifPlus :: Expr -> Expr -> Op
    BifMinus :: Expr -> Expr -> Op
    BifDivide :: Expr -> Expr -> Op
    RegSet  ::  Expr -> Op
    ObjGetBool  :: Expr -> Op
    deriving (Show,Eq)

deriving instance Show (Insn e x)

instance NonLocal (Insn) where
    entryLabel (Label l) = l 
    successors (Goto l) = [l]
    successors (CondBranch _ true false) = [true,false]

instance HooplNode (Insn) where
    mkBranchNode = Goto
    mkLabelNode = Label

-- map over all the expressions inside
-- BOILERPLATE
mapEO :: (Expr -> Expr) -> Op -> Op
mapEO func (BifPlus a b) = BifPlus (func a) (func b)
mapEO func (BifDivide a b) = BifDivide (func a) (func b)
mapEO func (BifMinus a b) = BifMinus (func a) (func b)
mapEO func (Subcall args)   = Subcall (map func args)
mapEO func (RegSet a) = RegSet (func a) 
mapEO func (Fetch a) = Fetch (func a)

mapE :: (Expr -> Expr) -> Insn e x -> Insn e x
mapE func (Op r op) = Op (r :: Reg) (mapEO func (op :: Op))
mapE func (CondBranch cond true false) = CondBranch (func cond) true false
mapE _ n@(Goto _) = n
mapE _ n@(Label _) = n

-- BOILERPLATE
exprs :: Op -> [Expr]
exprs (BifPlus a b) = [a,b]
exprs (BifDivide a b) = [a,b]
exprs (BifMinus a b) = [a,b]
exprs (Subcall args) = args
exprs (RegSet a) = [a] 
exprs (Fetch a) = [a]


-- convert an expression to a graph containing only it
insnToGraph :: Insn e x -> Graph Insn e x
insnToGraph n@(Op _ _)   = mkMiddle n
insnToGraph n@(CondBranch _ _ _)   = mkLast n
insnToGraph n@(Goto _)   = mkLast n
insnToGraph n@(Label _)   = mkFirst n

