import Nam
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T
import Data.Aeson.Parser;
import Data.Attoparsec
import Data.Aeson;
import Compiler.Hoopl
import Control.Monad.State.Strict
import System.Environment
mainLineNam = nam . head . xref
main = do
    [filename] <- getArgs
    namSource <- (B.readFile filename)
    let (Done rest r) = parse json namSource
    let (Success parsed) = (T.parse (parseJSON) r) :: (T.Result Unit)
    putStrLn $ show parsed
--    putStrLn $ show $ mainLineNam parsed
    let converted = fst $ evalState (convert $ mainLineNam parsed) 0
    putStrLn "\ngraph:"
    putStrLn $ showGraph ((++ "\n") . show) converted
