{-# LANGUAGE ViewPatterns #-}
import Data.Aeson.Parser;
import Data.Aeson;
import Data.Attoparsec
import Data.Attoparsec.Number
import Control.Monad
import Debug.Trace
import Data.Vector ((!))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T


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

data Op =
    Ann Integer Op
    | Prog [Op]
    | Fetch Op 
    | Const Op 
    | Unknow Value
    | StrLit String
    | Subcall [Op]
    | ScopedLex Op
    | Box String Op
    | Double Double
    deriving Show

str (String t) = T.unpack t
double (Number (D n)) = n
double (Number (I n)) = fromInteger n
double x = error $ show x
int (Number (I n)) = n
int x = error $ show x
--,_,,op
-- turns an raw array of opcodes into Op
rawOpsToOp (Array a) = case (V.toList a) of 
    [(str -> "ann"),_,(int -> pos),op] -> Ann pos (rawOpsToOp op)
    (str -> "prog"):rest -> Prog $ map rawOpsToOp rest
    [(str -> "fetch"),arg] -> Fetch $ rawOpsToOp arg
    [(str -> "const"),arg] -> Const $ rawOpsToOp arg
    [(str -> "scopedlex"),arg] -> ScopedLex $ rawOpsToOp arg
    [(str -> "box"),(str -> typeName),thing] -> Box typeName (rawOpsToOp thing)
    ((str -> "subcall"):sig:rest) -> Subcall (map rawOpsToOp rest)
    [(str -> "double"),(double -> val)] -> Double val
    other -> Unknow (Array $ V.fromList other)
rawOpsToOp other = Unknow other

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
    nam :: Op
    } | Other String
    deriving Show

parseXrefThing "sub" a = do
    subName_ <- parseJSON (a ! 1)
    return $ Sub {subName=subName_,nam=rawOpsToOp (a ! 17)}

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

main = do
    nam <- (B.readFile "MAIN.nam")
    let (Done rest r) = parse json nam
    let (Success parsed) = (T.parse (parseJSON) r) :: (T.Result Unit)

    putStrLn $ show parsed
