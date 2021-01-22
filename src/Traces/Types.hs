module Traces.Types where

import           Control.Monad.Reader           ( Reader )
import           Data.Map                       ( Map )

data Item = Marker | Letter Char deriving (Show)

data Env = Env { alph :: Alphabet, independent :: I, dependent :: D, word :: String } deriving (Show)
type REnv a = Reader Env a

type Alphabet = String
type Set = [(Char, Char)]
type I = Set
type D = Set
type FNF = [String]
type Trace = [String]
type Stacks = Map Char [Item]
type Edge = (Int, Int)
type Graph = ([Int], [Edge])
type Min = [Int]
