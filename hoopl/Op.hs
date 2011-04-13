{-# LANGUAGE ViewPatterns #-}
module Op (rawOpsToOp,Op(..)) where
import Data.Vector ((!))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T
import Data.Aeson;
import Data.Attoparsec.Number
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
    | Sink Op
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
    [(str -> "sink"),arg] -> Sink $ rawOpsToOp arg
    other -> Unknow (Array $ V.fromList other)
rawOpsToOp (String t) = StrLit (T.unpack t)
