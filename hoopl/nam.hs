import Nam
import ConstProp
import Insn
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson.Types as T
import Data.Aeson.Parser;
import Data.Attoparsec
import Data.Aeson;
import Compiler.Hoopl
import Control.Monad.State.Strict
import System.Environment
mainLineNam = nam . head . xref

type M = CheckingFuelMonad (SimpleUniqueMonad)

unmonad :: M a -> a
unmonad p = (runSimpleUniqueMonad $ runWithFuel 99999 p)

analyze :: (Graph Insn O O) -> M (Graph Insn O O)
analyze graph = do
    (graph,_,_) <- analyzeAndRewriteFwdOx constPropPass graph initFact
    return graph

putGraph :: Graph Insn e x -> IO ()
putGraph = putStrLn . showGraph ((++ "\n") . show)

main = do
    [filename] <- getArgs
    namSource <- (B.readFile filename)
    let (Done rest r) = parse json namSource
    let (Success parsed) = (T.parse (parseJSON) r) :: (T.Result Unit)
    putStrLn $ show parsed
--    putStrLn $ show $ mainLineNam parsed
    let converted = fst $ evalState (convert $ mainLineNam parsed) 0
    let graph = (unmonad (analyze converted))
    putStrLn "\norginal:"
    putGraph converted
    putStrLn "\nanalyzed:"
    putGraph graph

