module TemplateInstantiation.GraphReducerMk1 where

import Common.Language
import Common.Parser
import Common.Heap
import Common.ISeq
import TemplateInstantiation.TiStats
import Data.List (mapAccumL)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty((:|)), (<|))


-- Definition of the Template Instantiation state
-- type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStatsImpl)
data TiState = TiState
  { stack   :: TiStack
  , dump    :: TiDump
  , heap    :: TiHeap
  , globals :: TiGlobals
  , stats   :: TiStatsImpl 
  }
  deriving Show

-- Stack of addresses to nodes in the heap
type TiStack = NEL.NonEmpty Addr

{-
  A record of the state of the spine stack prior
  to the evaluation of an argument of a strict 
  primitive.

  (For this version we are not going to use it)
-}
data TiDump = DummyTiDump
  deriving Show

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
  deriving Show
-----------


runProg :: String -> String
runProg = showResults . eval . compile . parseInput

parseInput :: String -> CoreProgram
parseInput = syntax . clex 1

{-
  Initializes values for the global supercombinators,
  the stack (which is initially just the address of 'main'),
  the dump (here it is not used) and the heap of all 
  supercombinators present in the program.

  Returns a state with these values
-}
compile :: CoreProgram -> TiState
compile program 
  = TiState stack0 dump0 heap0 globals stats0
  where
    extraPreludeDefs = []
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (heap0, globals) = buildInitialHeap scDefs
    stack0 = addrOfMain :| []
    addrOfMain  | Just a <- lookup "main" globals = a
                | otherwise = error "main is not defined"
    dump0 = DummyTiDump
    stats0 = tiStatInitial

{-
  Takes an initial machine state and runs it one step at
  a time.

  Returns a list of all states. The first element is always
  the current state. If the current state is a final state,
  no more states are produced.
-}
eval :: TiState -> [TiState]
eval state = state : nextStates
  where
    doAdmin = applyToStats tiStatIncSteps
    nextState = doAdmin (step state)
    nextStates  | isTiFinal state = []
                | otherwise = eval nextState

isDataNode :: Node -> Bool 
isDataNode (NNum _) = True
isDataNode _        = False

isTiFinal :: TiState -> Bool
isTiFinal (TiState (soleAddr :| []) _ heap _ _) 
  = isDataNode (hLookup heap soleAddr)
isTiFinal _
  = False -- stack contains more than one item

-- step to the next state
step :: TiState -> TiState
step state @ (TiState (a :| as) _ heap _ _)
  = dispatch (hLookup heap a)
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSupercomb name args body) = scStep state name args body

-- A number should never be applied as a function
numStep _ _ = error "Number is being applied as a function"

{-
  State transition:

      TiState (a : s) d h[a : NAp a1 a2] f
  --> TiState (a1 : a : s) d h f
-}
apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = state {stack = stack'}
  where
    stack' = a1 <| stack state

{-
  State transition:

      TiState (a0 : a1 : .. : an : s) d h[a0 : NSupercomb name [x1 .. xn] body] f
  --> TiState (ar : s) d h' f
  where (h', ar) = instantiate body h f[x1 |-> a1 .. xn |-> an].

  Here, 'instantiate' is a function to instantiate the body of the supercombinator
-}
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName args body = 
  state {stack = ar :| rest, heap = heap'} 
  where
    n = length args
    h = heap state
    (scAndAps, rest) = NEL.splitAt (n + 1) (stack state)
    argsAddrs = getArgs h (NEL.fromList scAndAps)
    argsBindings = 
      if length (tail scAndAps) == n 
        then zip args argsAddrs
        else error "Function is applied to too few arguments"
    env = argsBindings ++ globals state
    (heap', ar) = instantiate body h env 

{-
  Gets the addresses of the argument of the supercombinator,
  whose address is located at the top of the stack
-}
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs h (sc :| stack) = map getArg stack
  where
    getArg addr = let (NAp fun arg) = hLookup h addr in arg

instantiate :: CoreExpr -- Body of the supercombinator
                -> TiHeap -- Heap before instantiation
                -> [(Name, Addr)] -- Association of name and addresses
                -> (TiHeap, Addr) -- Heap after instantiation and address of the root instance

instantiate (ENum n) heap env = hAlloc heap (NNum n)

instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env

instantiate (EVar name) heap env 
  | Just a <- lookup name env = (heap, a)
  | otherwise = error $ "Undefined name: " ++ name

instantiate e _ _ = 
  error $ "Can't instantiate the following expression yet: " ++ show e
-----------

-- Formating the results
showResults :: [TiState] -> String 
showResults states = display results 
  where
    results = iConcat
      [ iLayn (map showState states)
      , showStats (stats $ last states)
      ]
    display :: ISeqImpl -> String
    display = iDisplay

showState :: ISeq s => TiState -> s
showState (TiState stack _ heap _ _)
  = iConcat [showStack heap stack, iNewLine]

showStack :: ISeq s => TiHeap -> TiStack -> s
showStack heap (a :| as) 
  = iConcat 
      [ iStr "Stack ["
      , iInterleave iNewLine nodes
      , iStr " ]"
      ]
  where
    nodes = map showStackItem (a : as)
    showStackItem addr = 
      iConcat 
        [ showFWAddr addr, iStr ": "
        , showStackNode (hLookup heap addr) heap
        ]

showStackNode :: ISeq s => Node -> TiHeap -> s
showStackNode (NAp funAddr argAddr) heap
  = iConcat
      [ iStr "NAp ", showFWAddr funAddr
      , iStr " ", showFWAddr argAddr, iStr " ("
      , showNode (hLookup heap argAddr), iStr ")"
      ]
showStackNode node _ = showNode node 

showNode :: ISeq s => Node -> s
showNode (NAp a1 a2) =
  iConcat
    [ iStr "NAp ", addrToSeq a1
    , iStr " ", addrToSeq a2
    ]
showNode (NSupercomb name args body) =
  iStr ("NSupercomb " ++ name)
showNode (NNum n) = iAppend (iStr "NNum ") (iNum n)

addrToSeq :: ISeq s => Addr -> s 
addrToSeq addr = iStr (showAddr addr)

showFWAddr :: ISeq s => Addr -> s
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = showAddr addr

showStats :: (TiStats st, ISeq s) => st -> s
showStats stats = 
  iConcat 
    [ iNewLine, iNewLine, iStr "Total numbers of steps = "
    , iNum (tiStatGetSteps stats)
    ]
-----------

-- Helpers
applyToStats :: (TiStatsImpl -> TiStatsImpl) -> TiState -> TiState
applyToStats f state = state {stats = f (stats state)}

buildInitialHeap :: [ScDefn Name] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (ScDefn name args body) 
  = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)
-----------