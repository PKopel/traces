module Traces.Trace where

import           Data.List                      ( permutations )
import           Control.Monad.Reader           ( asks )
import           Traces.Types

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

computeD :: Alphabet -> I -> D
computeD alph ind = [ (x, y) | x <- alph, y <- alph, (x, y) `notElem` ind ]

computeTrace :: String -> REnv Trace
computeTrace w = do
  ind <- asks independent
  return $ filter (eqvI ind w) $ permutations w


eqvI :: I -> String -> String -> Bool
eqvI ind (a : t) (b : s) =
  let areIndependent = a == b || (a, b) `elem` ind
      areInOrder = count a (a : t) == count a s || count b (b : s) == count b t
  in  (areIndependent || areInOrder) && eqvI ind t s
eqvI _ [] [] = True
eqvI _ _  _  = False
