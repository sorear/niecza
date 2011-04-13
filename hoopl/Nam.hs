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

-- a side effect free expression
-- FIXME handle Box and Ann smartly
data Expr = Double Double |  StrLit String | ScopedLex Expr | Reg Int
    deriving Show
data Insn e x where
    Fetch :: Int -> Expr -> Insn O O
    Subcall :: Int -> [Expr] -> Insn O O

instance NonLocal (Insn) 
    



freshId = do
    id <- get
    put (id+1)
    return $ id

simple val = return $ (emptyGraph,val) 

convert :: Op.Op -> State Int ((Graph Insn O O),Expr)

-- ops which map directly to Expr

convert (Op.Double d) = simple $ Double d
convert (Op.StrLit str) = simple $ StrLit str
convert (Op.ScopedLex (Op.StrLit str)) = simple $ ScopedLex $ StrLit str

-- HACKS 
  
convert (Op.Ann _ op) = convert op
convert (Op.Box _ op) = convert op
convert (Op.Const op) = convert op


convert (Op.Prog ops) = do
    converted <- mapM convert ops
    let (setup,vals) = unzip converted
    return $ (foldl1 (<*>) setup,last vals)

convert (Op.Subcall args) = do
    converted <- mapM convert args
    let (setup,vals) = unzip converted
    id <- freshId
    return $ (((foldl1 (<*>) setup) <*> (mkMiddle $ Subcall id vals)),Reg id)

convert (Op.Fetch arg) = do
    id <- freshId
    (setup,val) <- convert arg
    return $ (setup <*> (mkMiddle $ Fetch id val),Reg id)

convert (Op.Sink arg) = convert arg

-- HACK those nodes shouldn't be ignored

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
    } | Other String
    deriving Show

parseXrefThing "sub" a = do
    subName_ <- parseJSON (a ! 1)
    return $ Sub {subName=subName_,nam=Op.rawOpsToOp (a ! 17)}

parseXrefThing thing_type _ = return $ Other thing_type

instance FromJSON XrefThing where
    parseJSON (Array a) = do
        thing_type <- parseJSON (a ! 0)
        parseXrefThing thing_type a
    parseJSON _ = mzero


instance FromJSON Xref where 
    parseJSON (Array a) = do
        unit  <- parseJSON (a ! 0)
        index <- parseJSON (a ! 1)
        xref_name <- parseJSON (a ! 2)
        return $ Xref { unit=unit, index=index, xref_name=xref_name }
        
    parseJSON _ = mzero

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
        xref <- parseJSON (a ! 7)
        return $ Unit {
            mainline_ref=mainline_ref,
            name=name,
            setting=setting,
            bottom_ref=bottom_ref,
            filename=filename,
            modtime=modtime,
            xref=xref
        }
    parseJSON _ = mzero

deriving instance Show (Insn e x)

--showNode x = "...;\n"
