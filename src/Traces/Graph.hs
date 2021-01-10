module Traces.Graph
  ( wordToGraph
  )
where

import           Control.Monad.Reader           ( asks )
import           Traces.Types                   ( Env(dependent)
                                                , Graph
                                                , Edge
                                                , Min
                                                , REnv
                                                )


wordToGraph :: String -> REnv Graph
wordToGraph word =
  wordToGraphLoop (length word) ([], (word, [], [])) >>= minimizeGraph [] . snd

wordToGraphLoop :: Int -> (Min, Graph) -> REnv (Min, Graph)
wordToGraphLoop 0 graph                    = return graph
wordToGraphLoop n (minSet, (word, vs, es)) = do
  (newMinSet, graph) <- minSetLoop (minSet, (word, n : vs, es)) [] n
  wordToGraphLoop (n - 1) (n : newMinSet, graph)


minSetLoop :: (Min, Graph) -> Min -> Int -> REnv (Min, Graph)
minSetLoop ([]         , graph               ) minSet _ = return (minSet, graph)
minSetLoop (a : minTail, graph@(word, vs, es)) minSet v = do
  dep <- asks dependent
  let la       = word !! (a - 1)
      lv       = word !! (v - 1)
      newGraph = if (la, lv) `elem` dep then (word, vs, (v, a) : es) else graph
  minSetLoop (minTail, newGraph) (a : minSet) v

minimizeGraph :: [Edge] -> Graph -> REnv Graph
minimizeGraph minEs (word, vs, []    ) = return (word, vs, minEs)
minimizeGraph minEs (word, vs, e : es) = do
  redundant <- checkEdge word e
  let newMinEs = if redundant then minEs else e : minEs
  minimizeGraph newMinEs (word, vs, es)

checkEdge :: String -> Edge -> REnv Bool
checkEdge word e@(a, _) = checkEdgeLoop word e a

checkEdgeLoop :: String -> Edge -> Int -> REnv Bool
checkEdgeLoop word (a, b) i
  | i == b - 1 = return False
  | otherwise = do
    dep <- asks dependent
    if redundant dep then return True else checkEdgeLoop word (a, b) (i + 1)
 where
  la = word !! (a - 1)
  lb = word !! (b - 1)
  li = word !! i
  redundant dep = (la, li) `elem` dep && (li, lb) `elem` dep

