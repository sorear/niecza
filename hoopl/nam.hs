import Data.Aeson.Parser;
import Data.Aeson;
import Data.Attoparsec
import Control.Monad
--import Text.Show.ByteString as X
import Data.Vector
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T;


data Xref = Xref {
    unit  :: String,  --  Names unit of origin
    index :: Integer, --  Indexes into unit's xref array
    xref_name  :: String  --  Descriptive name for debugging
}
data Unit = Unit {
--    mainline_ref ::   Xref,    -- Xref to mainline subroutine
      name         ::   String  -- Unit's unique name
--    log          ::   ...     -- Mostly unused vestige of last stash system
--    setting      ::   String,  -- Name of setting unit or null
--    bottom_ref   ::   Xref,    -- Xref to sub containing {YOU_ARE_HERE}, or null
--    filename     ::   String,  -- Filename of source code or null
--    modtime      ::   Integer -- Seconds since 1970-01-01
--    xref         ::   [Xref]  -- Resolves refs from other units
--    tdeps        ::   TDep    -- Holds dependency data for recompilation
--    stash_root   ::   StNode  -- Trie holding classes and global variables
} deriving Show
instance FromJSON Unit where 
    parseJSON (Array a) = do
        name <- parseJSON (a ! 1)
        return $ Unit {name=name}
--    parseJSON (Array a) = let [
--                                mainline_ref,
--                               name        ,
--                               log         ,
--                               setting     ,
--                               bottom_ref  ,
--                               filename    ,
--                               modtime     ,
--                               xref        ,
--                               tdeps       ,
--                               stash_root
--                               ] = toList a in return $ Unit mainline_ref setting bottom_ref filename modtime
--    parseJSON _ = mzero
main = do
    nam <- (B.readFile "MAIN.nam")
    let (Done rest r) = parse json nam
    let (Success parsed) = (T.parse (parseJSON) r) :: (T.Result Unit)

    putStrLn $ show parsed
