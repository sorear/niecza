{-# LANGUAGE NoMonomorphismRestriction #-}
module Util (toStr,M) where
import Compiler.Hoopl
import Data.Aeson;
import Data.Text.Encoding
import Data.Text (unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
type M = CheckingFuelMonad (SimpleUniqueMonad)
toStr =  unpack . decodeASCII . B.pack . BL.unpack . encode
