module Traces.FNF where

import           Data.Map                      as Map
                                                ( empty
                                                , insert
                                                , lookup
                                                , null
                                                )
import           Data.List                     as List
                                                ( foldl )
import           Control.Monad.Reader           ( foldM
                                                , asks
                                                )
import           Traces.Types

wordToFNF :: String -> REnv FNF
wordToFNF word = createStacks word >>= stacksToFNF

createStacks :: String -> REnv Stacks
createStacks = foldM placeMarkers Map.empty

placeMarkers :: Stacks -> Char -> REnv Stacks
placeMarkers stacks char = do
  alph <- asks alph
  dep  <- asks dependent
  return $ List.foldl
    (\newStacks c -> if (c, char) `elem` dep
      then
        let item = if c == char then Letter c else Marker
        in  placeMarker c item newStacks
      else newStacks
    )
    stacks
    alph

placeMarker :: Char -> Item -> Stacks -> Stacks
placeMarker c marker map = case Map.lookup c map of
  Nothing       -> Map.insert c [marker] map
  Just oldStack -> Map.insert c (marker : oldStack) map

stacksToFNF :: Stacks -> REnv FNF
stacksToFNF stacks = stacksToFNFAcc stacks []

stacksToFNFAcc :: Stacks -> FNF -> REnv FNF
stacksToFNFAcc stacks acc
  | Map.null stacks = return acc
  | otherwise = do
    rAlph               <- asks (reverse . alph)
    dep                 <- asks dependent
    (newStacks, newAcc) <- foldM findStep (stacks, acc) rAlph
    stacksToFNFAcc newStacks ([] : newAcc)

findStep :: (Stacks, FNF) -> Char -> REnv (Stacks, FNF)
findStep (stacks, acc@(step : rest)) char = case Map.lookup char stacks of
  Just ((Letter l) : lStack) -> rmMarkers stacks l >>= \newStacks ->
    return (Map.insert l lStack newStacks, (l : step) : rest)
  _ -> return (stacks, acc)

rmMarkers :: Stacks -> Char -> REnv Stacks
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
