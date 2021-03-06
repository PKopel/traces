module Traces.Trace
  ( complement
  , computeTrace
  )
where

import qualified Data.List                     as List
import           Control.Monad.Reader           ( asks )
import           Control.Monad                  ( filterM )
import           Traces.Types

complement :: Eq a => [a] -> [(a, a)] -> [(a, a)]
complement alph ind = [ (x, y) | x <- alph, y <- alph, (x, y) `notElem` ind ]

computeTrace :: REnv Trace
computeTrace = do
  word <- asks word
  ind  <- asks independent
  nc   <- findNonCommutable word
  List.nub <$> filterM (eqvI nc) (List.permutations word)

eqvI :: [String] -> String -> REnv Bool
eqvI nc string = null . (nc List.\\) <$> findNonCommutable string

findNonCommutable :: String -> REnv [String]
findNonCommutable = findNonCommutableAcc []

findNonCommutableAcc :: [String] -> String -> REnv [String]
findNonCommutableAcc ncLists []         = return ncLists
findNonCommutableAcc ncLists (a : word) = do
  dep <- asks dependent
  let nonCommutable = List.filter (\b -> (a, b) `elem` dep) word
      newNcLists    = List.sort (a : nonCommutable) : ncLists
  findNonCommutableAcc newNcLists word
