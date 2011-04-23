{-# LANGUAGE ViewPatterns #-}
module Op (rawOpsToOp,Op(..)) where
import Data.Vector ((!))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T
import Data.Aeson ;
import Data.Attoparsec.Number
import Util
data Op =
    Ann Integer Op
    | Prog [Op]
    | Fetch Op 
    | Const Op 
    | Unknown Value
    | StrLit String
    | Subcall [Op]
    | ScopedLex String
    | ScopedLexBind String Op
    | CoreLex String
    | Box String Op
    | Double Double
    | BifPlus Op Op
    | BifDivide Op Op
    | BifMinus Op Op
    | Sink Op
    | LetN [(String,Op)] Op
    | LetScope Bool [(String,Op)] Op
    | Ternary Op Op Op
    | ObjGetBool Op
    | LetVar String
    | Null String
    | NewBoundVar Integer Integer Op
    deriving Show

str (String t) = T.unpack t
double (Number (D n)) = n
double (Number (I n)) = fromInteger n
double x = error $ show x
int (Number (I n)) = n
int x = error $ show x
--,_,,op

pairsConvert ((str -> key):value:rest) = 
    (key,rawOpsToOp value):pairsConvert rest
pairsConvert [] = []
--pairsConvert other = error $ show other

-- turns an raw array of opcodes into Op
rawOpsToOp (Array a) = case (V.toList a) of 
    -- HACK
    (str -> "xspan"):_:_:_:body:rest -> rawOpsToOp body

    [(str -> "ann"),_,(int -> pos),op] -> Ann pos (rawOpsToOp op)
    (str -> "prog"):rest -> Prog $ map rawOpsToOp rest
    [(str -> "fetch"),arg] -> Fetch $ rawOpsToOp arg
    [(str -> "const"),arg] -> Const $ rawOpsToOp arg

    [(str -> "bif_plus"),a,b]   -> BifPlus (rawOpsToOp a) (rawOpsToOp b)
    [(str -> "bif_minus"),a,b]  -> BifMinus (rawOpsToOp a) (rawOpsToOp b)
    [(str -> "bif_divide"),a,b] -> BifDivide (rawOpsToOp a) (rawOpsToOp b)

    [(str -> "corelex"),(str -> a)] -> CoreLex a
    [(str -> "scopedlex"),(str -> a)] -> ScopedLex a
    [(str -> "scopedlex"),(str -> a),b] -> ScopedLexBind a (rawOpsToOp b)
    [(str -> "box"),(str -> typeName),thing] -> Box typeName (rawOpsToOp thing)
    ((str -> "subcall"):sig:rest) -> Subcall (map rawOpsToOp rest)
    [(str -> "double"),(double -> val)] -> Double val
    [(str -> "sink"),arg] -> Sink $ rawOpsToOp arg
    [(str -> "obj_getbool"),arg] -> ObjGetBool $ rawOpsToOp arg
    [(str -> "ternary"),cond,true,false] -> Ternary (rawOpsToOp cond) (rawOpsToOp true) (rawOpsToOp false)
    (str -> "letn"):args -> LetN (pairsConvert (init args)) (rawOpsToOp (last args))
    (str -> "letscope"):transparent:args -> LetScope True (pairsConvert (init args)) (rawOpsToOp (last args))
    [(str -> "letvar"),(str -> var)] -> LetVar var
    [(str -> "null"),(str -> var)] -> Op.Null var
    [(str -> "str"),(str -> val)] -> StrLit val
    [(str -> "newboundvar"),(int -> a),(int -> b),c] -> NewBoundVar a b (rawOpsToOp c)
    other -> error ("Can't convert to Op:" ++ (toStr other))
rawOpsToOp (String t) = StrLit (T.unpack t)
