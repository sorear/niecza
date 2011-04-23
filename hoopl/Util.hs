{-# LANGUAGE NoMonomorphismRestriction #-}
module Util (toStr) where
import Data.Aeson;
import Data.Text.Encoding
import Data.Text (unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
toStr =  unpack . decodeASCII . B.pack . BL.unpack . encode
