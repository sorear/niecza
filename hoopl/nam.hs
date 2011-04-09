import Nam
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T
import Data.Aeson.Parser;
import Data.Attoparsec
import Data.Aeson;
import Compiler.Hoopl
mainLineNam = nam . head . xref
main = do
    namSource <- (B.readFile "MAIN.nam")
    let (Done rest r) = parse json namSource
    let (Success parsed) = (T.parse (parseJSON) r) :: (T.Result Unit)
    putStrLn $ show parsed
--    putStrLn $ show $ mainLineNam parsed
    let converted = convert $ mainLineNam parsed
    putStrLn "\ngraph:"
    putStrLn $ showGraph ((++ "\n") . show) converted
