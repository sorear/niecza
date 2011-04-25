{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, NoMonomorphismRestriction,ViewPatterns #-}
module ConstProp (ConstFact, constLattice, initFact, varHasLit, constProp, constPropPass, M) where


--import Control.Monad
import Insn
import qualified Data.Map as Map
import Util

import Compiler.Hoopl
--import IR
--import OptSupport

-- ConstFact:
--   Not present in map => bottom
--   PElem v => variable has value v
--   Top     => variable's value is not constant
-- Type and definition of the lattice

type ConstFact = Map.Map Reg (WithTop Expr)
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice
 { fact_name = "Constant propagation"
 , fact_bot  = Map.empty
 , fact_join = joinMaps (extendJoinDomain constFactAdd) }
 where
   constFactAdd _ (OldFact old) (NewFact new) 
       = if new == old then (NoChange, PElem new)
         else               (SomeChange, Top)

-- Initially, we assume that all variable values are unknown.

-- Only interesting semantic choice: values of variables are live across
-- a call site.
-- Note that we don't need a case for x := y, where y holds a constant.
-- We can write the simplest solution and rely on the interleaved optimization.
--------------------------------------------------
-- Analysis: variable equals a literal constant
varHasLit :: FwdTransfer Insn ConstFact
varHasLit = mkFTransfer3 hack1 ft hack2 -- HACK: we don't have thsoe node types yet
 where
  ft :: Insn O O -> ConstFact ->  ConstFact

  ft (RegSet reg constant@(Double _)) f = Map.insert reg (PElem constant) f
  ft (insnTarget -> Just reg)  f = (Map.insert reg Top f)
  ft _ f = f

  hack1 _ f = f
  hack2 _ _ = noFacts

constPropPass :: FwdPass M Insn ConstFact
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = constProp `thenFwdRw` simplify}

initFact :: ConstFact
initFact = Map.fromList []

singleInsn :: (Monad m) => Insn e x -> m (Maybe (Graph Insn e x))
singleInsn = return . Just . insnToGraph

constProp :: FuelMonad m => FwdRewrite m Insn ConstFact
constProp = mkFRewrite cp
 where
    cp :: (Monad m) => Insn e x -> ConstFact -> m (Maybe (Graph Insn e x))
    cp insn f = singleInsn $ mapE (lookup f) insn
    lookup f reg@(Reg r)  = case Map.lookup r f of
        Just (PElem c) -> c 
        _ -> reg
    lookup _ x  = x



--insnToG :: Insn e x -> Graph Insn e x

simplify :: FuelMonad m => FwdRewrite m Insn ConstFact
simplify = mkFRewrite s
 where
    s :: (Monad m) => Insn e x -> a -> m (Maybe (Graph Insn e x))
    s (BifPlus reg (Double a) (Double b)) _ = singleInsn $ RegSet reg (Double (a+b))
    s (BifMinus reg (Double a) (Double b)) _ = singleInsn $ RegSet reg (Double (a-b))
    s (BifDivide reg (Double a) (Double b)) _ = singleInsn $ RegSet reg (Double (a/b))
    s _ _ = return Nothing


