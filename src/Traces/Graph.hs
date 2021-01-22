module Traces.Graph
  ( wordToGraph
  )
where

import           Control.Monad.Reader           ( asks )
import           Traces.Types

wordToGraph :: REnv Graph
wordToGraph = do
  word <- asks word
  wordToGraphLoop (length word) ([], ([], [])) >>= minimizeGraph [] . snd

wordToGraphLoop :: Int -> (Min, Graph) -> REnv (Min, Graph)
wordToGraphLoop 0 graph              = return graph
wordToGraphLoop n (minSet, (vs, es)) = do
  (newMinSet, graph) <- minSetLoop (minSet, (n : vs, es)) [] n
  wordToGraphLoop (n - 1) (n : newMinSet, graph)


minSetLoop :: (Min, Graph) -> Min -> Int -> REnv (Min, Graph)
minSetLoop ([]         , graph         ) minSet _ = return (minSet, graph)
minSetLoop (a : minTail, graph@(vs, es)) minSet v = do
  word <- asks word
  dep  <- asks dependent
  let la       = word !! (a - 1)
      lv       = word !! (v - 1)
      newGraph = if (la, lv) `elem` dep then (vs, (v, a) : es) else graph
  minSetLoop (minTail, newGraph) (a : minSet) v

minimizeGraph :: [Edge] -> Graph -> REnv Graph
minimizeGraph minEs (vs, []    ) = return (vs, minEs)
minimizeGraph minEs (vs, e : es) = do
  redundant <- checkEdge e
  let newMinEs = if redundant then minEs else e : minEs
  minimizeGraph newMinEs (vs, es)

checkEdge :: Edge -> REnv Bool
checkEdge e@(a, _) = checkEdgeLoop e a

checkEdgeLoop :: Edge -> Int -> REnv Bool
checkEdgeLoop (a, b) i
  | i == b - 1 = return False
  | otherwise = do
    word <- asks word
    dep  <- asks dependent
    let la = word !! (a - 1)
        lb = word !! (b - 1)
        li = word !! i
        redundant dep = (la, li) `elem` dep && (li, lb) `elem` dep
    if redundant dep then return True else checkEdgeLoop (a, b) (i + 1)


