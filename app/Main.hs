module Main where

import           System.Environment             ( getArgs )
import           Control.Monad.Reader           ( runReader )
import           Traces.Types
import           Traces.Trace                   ( complement
                                                , computeTrace
                                                )
import           Traces.Graph                   ( wordToGraph )
import           Traces.FNF.Word                ( wordToFNF )
import           Traces.FNF.Graph               ( graphToFNF )
import           Utils

main :: IO ()
main = do
  (file : _)         <- getArgs
  (alph, iSet, word) <- parseFile file
  let dSet         = complement alph iSet
      env          = Env alph iSet dSet
      trace        = runReader (computeTrace word) env
      fnfFromWord  = runReader (wordToFNF word) env
      graph        = runReader (wordToGraph word) env
      fnfFromGraph = graphToFNF graph
  saveResults dSet trace fnfFromWord graph fnfFromGraph

parseFile :: FilePath -> IO (Alphabet, I, String)
parseFile file = do
  contents <- readFile file
  let (alph : inISet : word : _) = lines contents
      iSet                       = parseISet inISet
  return (alph, iSet, word)

saveResults :: D -> Trace -> FNF -> Graph -> FNF -> IO ()
saveResults dSet trace wordFNF graph graphFNF = do
  let dSetString     = setToString dSet
      traceString    = show trace
      wordFNFString  = fnfToString wordFNF
      graphString    = graphToString graph
      graphFNFString = fnfToString graphFNF
  writeFile "results.txt" $ unlines
    [dSetString, traceString, wordFNFString, graphString, graphFNFString]
