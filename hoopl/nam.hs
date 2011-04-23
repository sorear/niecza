import Nam
import ConstProp
import DeadRegs
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


unmonad :: M a -> a
unmonad p = (runSimpleUniqueMonad $ runWithFuel 99999 p)

analyze :: (Graph Insn O O) -> M (Graph Insn O O)
analyze graph = do
    (graph,_,_) <- analyzeAndRewriteFwdOx constPropPass graph initFact
    return graph

analyzeBwd :: (Graph Insn O O) -> M (Graph Insn O O)
analyzeBwd graph = do
    (graph2,_,_) <- analyzeAndRewriteBwdOx deadRegsPass graph deadRegsInitFact
    return graph2

putGraph :: Graph Insn e x -> IO ()
putGraph = putStrLn . showGraph ((++ "\n") . show)

parseUnit json = case ((T.parse (parseJSON) json) :: (T.Result Unit)) of
    Success unit -> unit
    Error msg -> error msg

main = do
    [filename] <- getArgs
    namSource <- (B.readFile filename)
    let (Done rest r) = parse json namSource
    let parsed = parseUnit r
    putStrLn $ show parsed
--    putStrLn $ show $ mainLineNam parsed
    let converted = fst $ evalState (convert $ mainLineNam parsed) 0
    let graph = (unmonad (analyze converted))
    let graph2 = (unmonad (analyzeBwd graph))
    putStrLn "\norginal:"
    putGraph converted
    putStrLn "\nfirst pass:"
    putGraph graph
    putStrLn "\nsecond pass:"
    putGraph graph2

