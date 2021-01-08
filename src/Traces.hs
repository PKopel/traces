module Traces where

import Data.Map as Map ( empty, insert, lookup, null, Map )
import Data.List as List ( foldl )
import Control.Monad.Reader ( foldM, asks, Reader )

data Item = Marker | Letter Char

data Env = Env { alph :: Alphabet, independent :: I, dependent :: D}
type REnv a = Reader Env a

type Alphabet = String
type Set = [(Char, Char)]
type I = Set
type D = Set
type FNF = [String]

computeD :: Alphabet -> I -> D
computeD alph ind = [(x,y) | x <- alph, y <- alph, (x,y) `notElem` ind]

wordToFNF :: String -> REnv FNF
wordToFNF word = createStacks word >>= stacksToFNF

createStacks :: String -> REnv (Map.Map Char [Item])
createStacks = foldM placeMarkers Map.empty

placeMarkers :: Map.Map Char [Item] -> Char -> REnv (Map.Map Char [Item])
placeMarkers map char = do
  alph <- asks alph
  ind  <- asks independent
  return $ List.foldl
    (\newMap c ->
      let item = if c == char || (char, c) `elem` ind then Letter c else Marker
      in  placeMarker c item newMap
    )
    map
    alph

placeMarker :: Char -> Item -> Map.Map Char [Item] -> Map.Map Char [Item]
placeMarker c marker map = case Map.lookup c map of
  Nothing       -> Map.insert c [marker] map
  Just oldStack -> Map.insert c (marker : oldStack) map

stacksToFNF :: Map.Map Char [Item] -> REnv FNF
stacksToFNF stacks = stacksToFNFAcc stacks []

stacksToFNFAcc :: Map.Map Char [Item] -> FNF -> REnv FNF
stacksToFNFAcc stacks acc
  | Map.null stacks = return acc
  | otherwise = do
    rAlph               <- asks (reverse . alph)
    dep                 <- asks dependent
    (newStacks, newAcc) <- foldM findStep (stacks, acc) rAlph
    stacksToFNFAcc newStacks ([] : newAcc)

findStep
  :: (Map.Map Char [Item], FNF) -> Char -> REnv (Map.Map Char [Item], FNF)
findStep (stacks, acc@(step : rest)) char = case Map.lookup char stacks of
  Just ((Letter l) : lStack) -> rmMarkers stacks l >>= \newStacks ->
    return (Map.insert l lStack newStacks, (l : step) : rest)
  _ -> return (stacks, acc)

rmMarkers :: Map.Map Char [Item] -> Char -> REnv (Map.Map Char [Item])
rmMarkers stacks char = do
  rAlph <- asks (reverse . alph)
  dep   <- asks dependent
  return $ List.foldl
    (\newStacks c -> if (char, c) `elem` dep
      then case Map.lookup c newStacks of
        Just (Marker : rest) -> Map.insert c rest newStacks
        _                    -> newStacks
      else newStacks
    )
    stacks
    rAlph
