{-# LANGUAGE NamedFieldPuns #-}

module Common.Heap where

import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty((:|)))

{-
  Heap and Addr are used to represent a garbage-collected
  heap of nodes
-}
class Heap h where
  
  -- Initialized empty heap
  hInitial :: h a
  
  {-
    Takes a heap and an element, and returns
    a new heap and the address of such element
  -}
  hAlloc :: h a -> a -> (h a, Addr)

  {-
    Takes a heap, an address and an element, and
    returns a new heap with the address updated,
    replacing the associated element with the given one
  -}
  hUpdate :: h a -> Addr -> a -> h a

  {-
    Returns a new heap with the element removed
    at the specified address
  -}
  hFree :: h a -> Addr -> h a 


  -- Returns the element associated to the given address
  hLookup :: h a -> Addr -> a 

  -- Returns all the addresses of the given heap
  hAddresses :: h a -> [Addr]

  -- Returns the number of elements in the given heap
  hSize :: h a -> Int


data HeapImpl a = HeapImpl
  { size :: Int
  , unused :: NEL.NonEmpty Addr
  , mapping :: [(Addr, a)]
  }

instance Heap HeapImpl where

  hInitial = HeapImpl 0 (1 :| [2..]) []

  hAlloc HeapImpl {size, unused = next :| free, mapping} e = 
    (h, next)  
    where
      h = HeapImpl (size + 1) (NEL.fromList free) ((next, e) : mapping)

  hUpdate HeapImpl {size, unused, mapping} a e = 
    HeapImpl size unused mapping'
    where
      mapping' = (a, e) : removeAddr a mapping

  hFree HeapImpl {size, unused, mapping} a = 
    HeapImpl size unused (removeAddr a mapping)

  hLookup HeapImpl {size, unused, mapping} a 
    | Just mp <- lookup a mapping = mp
    | otherwise = error $ 
      "can't find node with the address " 
      ++ showAddr a 
      ++ " in the heap"

  hAddresses h = map fst (mapping h)

  hSize = size


type Addr = Int

{-
  Returns an address that is guaranteed to differ
  from every address returned by hAlloc
-}
hNull :: Addr
hNull = 0 

-- Tells whether the address was returned by hNull
hIsNull :: Addr -> Bool 
hIsNull = (== 0) 

removeAddr :: Addr -> [(Addr, a)] -> [(Addr, a)]
removeAddr a [] = error $ "Attempting to update or free a non-existent address: " ++ showAddr a
removeAddr a ((a1, e) : mapping) 
  | a1 == a   = mapping
  | otherwise = (a1, e) : removeAddr a mapping 

showAddr :: Addr -> String
showAddr a = '#' : show a