module Utils
  ( parseISet
  , setToString
  , fnfToString
  , graphToString
  )
where

import           Traces.Types
import           Data.Char                      ( isLetter )

parseISet :: String -> I
parseISet = parseISetAcc [] . filter isLetter

parseISetAcc :: I -> String -> I
parseISetAcc iSet []               = iSet
parseISetAcc iSet (a : b : string) = parseISetAcc ((a, b) : iSet) string

setToString :: Set -> String
setToString = setToStringAcc "]"

setToStringAcc :: String -> Set -> String
setToStringAcc (',' : string) [] = '[' : string
setToStringAcc string ((a, b) : set) = setToStringAcc (addTuple string) set
  where addTuple string = ',' : '(' : a : ',' : b : ')' : string

fnfToString :: FNF -> String
fnfToString = fnfToStringAcc [] . reverse

fnfToStringAcc :: String -> FNF -> String
fnfToStringAcc string []           = string
fnfToStringAcc string (step : fnf) = fnfToStringAcc (addStep string) fnf
  where addStep string = '(' : step ++ ')' : string

graphToString :: Graph -> String
graphToString graph@(_, _, es) = graphToStringLabelsAcc edgesString graph
  where edgesString = graphToStringEdgesAcc "digraph g{\n" es

graphToStringEdgesAcc :: String -> [Edge] -> String
graphToStringEdgesAcc string []            = string
graphToStringEdgesAcc string ((a, b) : es) = graphToStringEdgesAcc
  (string ++ edgeString)
  es
  where edgeString = '\t' : show a ++ " -> " ++ show b ++ "\n"

graphToStringLabelsAcc :: String -> Graph -> String
graphToStringLabelsAcc string (_, []    , _ ) = string ++ "}"
graphToStringLabelsAcc string (w, v : vs, es) = graphToStringLabelsAcc
  (string ++ labelString)
  (w, vs, es)
  where labelString = '\t' : show v ++ "[label=" ++ (w !! (v - 1)) : "]\n"
