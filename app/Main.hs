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
      env          = Env alph iSet dSet word
      trace        = runReader computeTrace env
      fnfFromWord  = runReader wordToFNF env
      graph        = runReader wordToGraph env
      fnfFromGraph = runReader (graphToFNF graph) env
  saveResults word dSet trace fnfFromWord graph fnfFromGraph

parseFile :: FilePath -> IO (Alphabet, I, String)
parseFile file = do
  contents <- readFile file
  let (alph : inISet : word : _) = lines contents
      iSet                       = parseISet inISet
  return (alph, iSet, word)

saveResults :: String -> D -> Trace -> FNF -> Graph -> FNF -> IO ()
saveResults word dSet trace wordFNF graph graphFNF = do
  let dSetString     = setToString dSet
      traceString    = show trace
      wordFNFString  = fnfToString wordFNF
      graphString    = graphToString word graph
      graphFNFString = fnfToString graphFNF
  writeFile "results.txt" $ unlines
    [dSetString, traceString, wordFNFString, graphString, graphFNFString]
