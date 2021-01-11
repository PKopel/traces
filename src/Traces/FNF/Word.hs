module Traces.FNF.Word where

import qualified Data.Map                      as Map
import qualified Data.List                     as List
                                                ( foldl
                                                , null
                                                )
import           Control.Monad.Reader           ( foldM
                                                , asks
                                                )
import           Traces.Types

wordToFNF :: String -> REnv FNF
wordToFNF word = createStacks word >>= stacksToFNF

createStacks :: String -> REnv Stacks
createStacks word = Map.map reverse <$> foldM placeMarkers Map.empty word

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
  | Map.foldl (\b is -> b && List.null is) True stacks = return $ reverse acc
  | otherwise = do
    rAlph     <- asks (reverse . alph)
    dep       <- asks dependent
    newAcc    <- foldM (findStep stacks) ([] : acc) rAlph
    newStacks <- foldM rmMarkers stacks (head newAcc)
    stacksToFNFAcc newStacks newAcc

findStep :: Stacks -> FNF -> Char -> REnv FNF
findStep stacks acc@(step : rest) char =
  return $ case Map.lookup char stacks of
    Just ((Letter l) : _) -> (l : step) : rest
    _                     -> acc

rmMarkers :: Stacks -> Char -> REnv Stacks
rmMarkers stacks char = do
  rAlph <- asks (reverse . alph)
  dep   <- asks dependent
  return $ List.foldl
    (\newStacks c -> if (char, c) `elem` dep
      then case Map.lookup c newStacks of
        Just (Marker : rest) -> Map.insert c rest newStacks
        Just (Letter l : rest) ->
          if l == char then Map.insert c rest newStacks else newStacks
        _ -> newStacks
      else newStacks
    )
    stacks
    rAlph
