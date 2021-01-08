module Traces where

import           Data.Map                      as Map
                                                ( empty
                                                , insert
                                                , lookup
                                                , null
                                                , Map
                                                )
import           Data.List                     as List
                                                ( foldl
                                                , permutations
                                                )
import           Control.Monad.Reader           ( foldM
                                                , asks
                                                , Reader
                                                )

data Item = Marker | Letter Char

data Env = Env { alph :: Alphabet, independent :: I, dependent :: D}
type REnv a = Reader Env a

type Alphabet = String
type Set = [(Char, Char)]
type I = Set
type D = Set
type FNF = [String]
type Trace = [String]
type Stacks = Map Char [Item]

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

computeD :: Alphabet -> I -> D
computeD alph ind = [ (x, y) | x <- alph, y <- alph, (x, y) `notElem` ind ]

computeTrace :: String -> REnv Trace
computeTrace w = do
  ind <- asks independent
  return $ filter (eqvI ind w) $ List.permutations w


eqvI :: I -> String -> String -> Bool
eqvI ind (a : t) (b : s) =
  let areIndependent = a == b || (a, b) `elem` ind
      areInOrder = count a (a : t) == count a s || count b (b : s) == count b t
  in  (areIndependent || areInOrder) && eqvI ind t s
eqvI _ [] [] = True
eqvI _ _  _  = False

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
