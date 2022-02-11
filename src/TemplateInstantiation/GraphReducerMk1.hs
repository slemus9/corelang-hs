module TemplateInstantiation.GraphReducerMk1 where

import Common.Language
import Common.Parser
import Common.Heap
import TemplateInstantiation.TiStats

-- Definition of the Template Instantiation state
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStatsImpl)

-- Stack of addresses to nodes in the heap
type TiStack = [Addr]

{-
  A record of the state of the spine stack prior
  to the evaluation of an argument of a strict 
  primitive.

  (For this version we are not going to use it)
-}
data TiDump = DummyTiDump

-- A collection of tagged nodes
type TiHeap = HeapImpl Node

{-
  A collection of addresses of heap nodes
  of the supercombinators or primitives.

  Associates the supercombinator's name with 
  its address
-}
type TiGlobals = [(Name, Addr)]

-- Heap nodes
data Node = NAp Addr Addr -- Application
  | NSupercomb Name [Name] CoreExpr -- Supercombinator
  | NNum Int -- A number
-----------



runProg :: String -> String
runProg = showResults . eval . compile . parseInput

parseInput :: String -> CoreProgram
parseInput = undefined 

compile :: CoreProgram -> TiState
compile = undefined 

eval :: TiState -> [TiState]
eval = undefined 

showResults :: [TiState] -> String 
showResults = undefined 