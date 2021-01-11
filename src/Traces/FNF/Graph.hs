module Traces.FNF.Graph
  ( graphToFNF
  )
where

import qualified Data.List                     as List
import           Traces.Types                   ( FNF
                                                , Graph
                                                , Edge
                                                )

graphToFNF :: Graph -> FNF
graphToFNF = graphToFNFAcc []

graphToFNFAcc :: FNF -> Graph -> FNF
graphToFNFAcc acc (_, [], _) = List.reverse acc
graphToFNFAcc acc graph      = graphToFNFAcc (sortedStep : acc) newGraph
 where
  (newGraph, step) = findStep graph
  sortedStep       = List.sort step

findStep :: Graph -> (Graph, String)
findStep graph@(word, _, es) = (newGraph, step)
 where
  freeVs   = findVertexes graph
  step     = List.map ((word !!) . subtract 1) freeVs
  newGraph = List.foldl removeVertex graph freeVs

findVertexes :: Graph -> [Int]
findVertexes (_, vs, []         ) = vs
findVertexes (w, vs, (_, v) : es) = findVertexes (w, List.delete v vs, es)

removeVertex :: Graph -> Int -> Graph
removeVertex (w, vs, es) v = (w, newVs, newEs)
 where
  newVs = List.delete v vs
  newEs = List.filter ((/= v) . fst) es
