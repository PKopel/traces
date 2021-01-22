module Traces.FNF.Graph
  ( graphToFNF
  )
where

import qualified Data.List                     as List
import           Control.Monad.Reader           ( asks )
import           Traces.Types
graphToFNF :: Graph -> REnv FNF
graphToFNF = graphToFNFAcc []

graphToFNFAcc :: FNF -> Graph -> REnv FNF
graphToFNFAcc acc ([], _) = return $ List.reverse acc
graphToFNFAcc acc graph   = do
  (newGraph, step) <- findStep graph
  graphToFNFAcc (List.sort step : acc) newGraph

findStep :: Graph -> REnv (Graph, String)
findStep graph@(_, es) = do
  word <- asks word
  let freeVs   = findVertexes graph
      step     = List.map ((word !!) . subtract 1) freeVs
      newGraph = List.foldl removeVertex graph freeVs
  return (newGraph, step)

findVertexes :: Graph -> [Int]
findVertexes (vs, []         ) = vs
findVertexes (vs, (_, v) : es) = findVertexes (List.delete v vs, es)

removeVertex :: Graph -> Int -> Graph
removeVertex (vs, es) v = (newVs, newEs)
 where
  newVs = List.delete v vs
  newEs = List.filter ((/= v) . fst) es
