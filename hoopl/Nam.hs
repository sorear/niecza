{-# LANGUAGE ViewPatterns,GADTs,StandaloneDeriving,NoMonomorphismRestriction #-}
module Nam  where
import Data.Aeson;
import Data.Attoparsec.Number
import Control.Monad
import Debug.Trace
import Data.Vector ((!))
import Compiler.Hoopl
import qualified Op
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Insn
import Util
import qualified Data.Map as Map


    

type CM  = StateT ConvertState M
data ConvertState = ConvertState {
    uniqueReg :: Int,
    letVars :: Map.Map String Reg
}


freshID :: CM Reg
freshID = do
    state <- get
    put (state{uniqueReg=uniqueReg state+1})
    return $ uniqueReg state

simple val = return $ (emptyGraph,val) 

-- TODO: pick better name?
composit args func = do
    converted <- mapM convert args
    let (setup,vals) = unzip converted
    (extraSetup,ret) <- func vals
    return ((foldl (<*>) emptyGraph  setup) <*> extraSetup,ret)

basicInsn :: [Op.Op] -> (Reg -> [Expr] -> Insn O O) ->  CM ((Graph Insn O O),Expr)

basicInsn args transform = do
    id <- freshID
    composit args (\vals ->
        return (mkMiddle $ transform id vals,Reg id))

branch :: Expr -> Label -> Label -> AGraph Insn O C
branch cond' trueLabel falseLabel = 
    aGraphOfGraph $ mkLast $ CondBranch cond' trueLabel falseLabel

convert :: Op.Op -> CM ((Graph Insn O O),Expr)

-- ops which map directly to Expr

convert (Op.Double d) = simple $ Double d
convert (Op.StrLit str) = simple $ StrLit str
convert (Op.ScopedLex str) = simple $ ScopedLex str
convert (Op.CoreLex str) = simple $ CoreLex str
convert (Op.LetVar str) = simple $ StrLit $ "let var" ++ (show str)

-- HACKS 
  
convert (Op.Ann _ op) = convert op
convert (Op.Box _ op) = convert op
convert (Op.Const op) = convert op


convert (Op.Prog ops) = composit ops (\vals -> return (emptyGraph,last vals))

convert (Op.Subcall args) = basicInsn args Subcall

convert (Op.Fetch arg) = basicInsn [arg] (\reg [arg] -> Fetch reg arg)
convert (Op.ObjGetBool arg) = basicInsn [arg] (\reg [arg] -> ObjGetBool reg arg)

convert (Op.BifPlus a b) = basicInsn [a,b] (\reg [a,b] -> BifPlus reg a b)

convert (Op.BifDivide a b) = basicInsn [a,b] (\reg [a,b] -> BifDivide reg a b)

convert (Op.BifMinus a b) = basicInsn [a,b] (\reg [a,b] -> BifMinus reg a b)

convert (Op.Sink arg) = convert arg

convert (Op.Ternary cond true false) = do
    result <- freshID
    (condSetup',cond') <- convert cond
    (trueSetup',true') <- convert true
    (falseSetup',false') <- convert false
    ifStmt <- lift $ graphOfAGraph $ mkIfThenElse (branch cond') (aGraphOfGraph trueSetup') ((aGraphOfGraph falseSetup') :: AGraph Insn O O)
    return $ (condSetup' <*> ifStmt,Reg result)


convert (Op.LetN pairs body) = 
    let (regs,values) = unzip pairs in composit values (\
        expr -> convert body)



-- HACK those nodes shouldn't be ignored

convert (Op.Unknown value) = error $ "Can't convert: " ++ (toStr value)
convert (other) = error $ "Can't convert: " ++ (show other)

--convert (expr) = do
--    (setup,val) <- convertToExpr expr
--    return setup

data Xref = Xref {
    unit  :: String,  --  Names unit of origin
    index :: Integer, --  Indexes into unit's xref array
    xref_name  :: String  --  Descriptive name for debugging
} deriving Show

data Unit = Unit {
      mainline_ref ::   Xref,    -- Xref to mainline subroutine
      name         ::   String,  -- Unit's unique name
--    log          ::   ...     -- Mostly unused vestige of last stash system
    setting      ::   String,  -- Name of setting unit or null
    bottom_ref   ::   Maybe Xref,    -- Xref to sub containing {YOU_ARE_HERE}, or null
    filename     ::   String,  -- Filename of source code or null
    modtime      ::   Integer, -- Seconds since 1970-01-01
    xref         ::   [XrefThing]  -- Resolves refs from other units
--    tdeps        ::   TDep    -- Holds dependency data for recompilation
--    stash_root   ::   StNode  -- Trie holding classes and global variables
} deriving Show


    {-
data Sub = 
    name            string  Sub's name for backtraces
    outer_xref      Xref    OUTER:: sub, may be in a setting unit
    flags           number  [1]
    children        num[]   Supports tree traversals
    param_role_hack ...     [2]
    augment_hack    ...     [3]
    hint_hack       ...     [4]
    is_phaser       number  [5]
    body_of         Xref    Only valid in immediate block of class {} et al
    in_class        Xref    Innermost enclosing body_of
    cur_pkg         str[]   OUR:: as a list of names
    class           string  &?BLOCK.WHAT; "Sub" or "Regex"
    ltm             LtmNode Only for regexes; stores declarative prefix
    exports         str[][] List of global names
    signature       Param[] May be null in exotic cases
    lexicals        Lex[]   Come in multiple forms
    nam             ...     See description of opcodes earlier-}

data XrefThing = Sub {
    subName :: String,
    nam :: Op.Op
    } | Missing | Other String
    deriving Show

parseXrefThing "sub" a = do
    subName_ <- parseJSON (a ! 1)
    return $ Sub {subName=subName_,nam=Op.rawOpsToOp (a ! 17)}

parseXrefThing thing_type _ = return $ Other thing_type

instance FromJSON XrefThing where
    parseJSON (Array a) = do
        thing_type <- parseJSON (a ! 0)
        parseXrefThing thing_type a
    parseJSON Null = return $ Missing
    parseJSON other = fail ("Can't parse XrefThing:"++(show other))


instance FromJSON Xref where 
    parseJSON (Array a) = do
        unit  <- parseJSON (a ! 0)
        index <- parseJSON (a ! 1)
        xref_name <- parseJSON (a ! 2)
        return $ Xref { unit=unit, index=index, xref_name=xref_name }
        
    parseJSON other = fail ("Can't parse Xref:"++(show other))


instance FromJSON Unit where 
    parseJSON (Array a) = do
        mainline_ref <- parseJSON (a ! 0)
        name <- parseJSON (a ! 1)
        -- skipping log and setting 
        --
        setting <- parseJSON (a ! 3)
        bottom_ref <- parseJSON (a ! 4)
        filename <- parseJSON (a ! 5)
        modtime <- parseJSON (a ! 6)
        xref <- trace (toStr $ a ! 7) $ parseJSON (a ! 7)
        return $ Unit {
            mainline_ref=mainline_ref,
            name=name,
            setting=setting,
            bottom_ref=bottom_ref,
            filename=filename,
            modtime=modtime,
            xref=xref
        }
    parseJSON other = fail ("Can't parse Unit:"++(show other))


--showNode x = "...;\n"
