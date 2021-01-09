module Traces.Graph where

import           Control.Monad.Reader           ( asks )
import           Traces.Types


wordToGraph :: String -> REnv Graph
wordToGraph word = wordToGraphLoop word (length word) ([], ([], [])) >>= minimizeGraph . snd

wordToGraphLoop :: String -> Int -> (Min, Graph) -> REnv (Min, Graph)
wordToGraphLoop _ 0 graph = return graph
wordToGraphLoop word n (minSet, (vs, es)) = do
  (newMinSet,graph) <- minSetLoop (minSet, (n : vs, es)) [] n word 
  wordToGraphLoop word (n-1) (n:newMinSet,graph)


minSetLoop :: (Min, Graph) -> Min -> Int -> String -> REnv (Min, Graph)
minSetLoop ([]         , graph         ) minSet _ _ = return (minSet, graph)
minSetLoop (a : minTail, graph@(vs, es)) minSet v word= do
  dep <- asks dependent
  let la = word !! (a-1)
      lv = word !! (v-1)
      newGraph= if (la, lv) `elem` dep 
        then (vs, (v, a) : es)
        else graph
  minSetLoop (minTail, newGraph) (a:minSet) v word

minimizeGraph :: Graph -> REnv Graph 
minimizeGraph = undefined 
