module Traces.FNF.Graph where

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
  candidates = findVertexes graph []
  freeVs     = checkVertexes es candidates
  step       = List.map ((word !!) . subtract 1) freeVs
  newGraph   = List.foldl removeVertex graph freeVs

findVertexes :: Graph -> [Int] -> [Int]
findVertexes (_, vs, []) []     = vs
findVertexes (_, _ , []) freeVs = List.nub freeVs
findVertexes (w, vs, (v, _) : es) freeVs =
  findVertexes (w, vs, es) (v : freeVs)

checkVertexes :: [Edge] -> [Int] -> [Int]
checkVertexes []            freeVs = freeVs
checkVertexes ((_, v) : es) freeVs = checkVertexes es $ List.delete v freeVs

removeVertex :: Graph -> Int -> Graph
removeVertex (w, vs, es) v = (w, newVs, newEs)
 where
  newVs = List.delete v vs
  newEs = List.filter ((/= v) . fst) es
