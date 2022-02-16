module TemplateInstantiation.TiStats where

{-
  A type class meant to specify how to
  collect statistics of the run-time performance
  of the machine
-}
class TiStats s where

  tiStatInitial :: s
  tiStatIncSteps :: s -> s
  tiStatGetSteps :: s -> Int

newtype TiStatsImpl = TiStatsImpl Int
  deriving Show

instance TiStats TiStatsImpl where

  tiStatInitial = TiStatsImpl 0
  tiStatIncSteps (TiStatsImpl s) = TiStatsImpl (s + 1)
  tiStatGetSteps (TiStatsImpl s) = s
