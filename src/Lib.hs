{-# LANGUAGE BangPatterns #-}

module Lib where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC

-- Generate from exponential distribution
exponential :: PrimMonad m => Gen (PrimState m) -> m Double
exponential rng = (negate . log . (1.0 - )) <$> (uniform rng)

-- Instantaneous state of a Markovian queue
-- The double is the time, and the int is the occupancy.
data MarkovQueueState = MarkovQueueState !Double !Int
instance Show MarkovQueueState where
  show (MarkovQueueState t n) = show t ++ " " ++ show n

-- A markov queue, parameterised by the arrival and departure rate.
data MarkovQueue = MarkovQueue !Double !Double

-- Events are tagged as being either arrivals or departures.
data EventType = Arrival | Departure
                 deriving Eq

-- Update a markovian queue's state
update :: PrimMonad m =>
          MarkovQueueState  ->
          MarkovQueue       ->
          Gen (PrimState m) ->
          m MarkovQueueState
update !(MarkovQueueState t n) (MarkovQueue lambda mu) rng = do
  -- Generate the event time
  e <- fmap (/(lambda + mu)) $ exponential rng
  let t' = t + e

  -- Generate the event type
  u <- uniform rng
  let eventType = if n == 0 || u < lambda/(lambda+mu)
                  then Arrival
                  else Departure

  let n' = if eventType == Arrival then n+1 else n-1
  return $ MarkovQueueState t' n'


-- Update many times.
run :: Int
    -> Int
    -> MarkovQueueState
    -> MarkovQueue
    -> Gen RealWorld
    -> IO MarkovQueueState
run !i !steps !state queue rng
  | i == steps-1  = do
                      print state
                      return state
  | otherwise     = do
                      print state
                      state' <- update state queue rng
                      run (i+1) steps state' queue rng

