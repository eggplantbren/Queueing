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

-- A markov queue, parameterised by the arrival rate,
-- service rate, and number of servers.
data MarkovQueue = MarkovQueue {
                     arrivalRate :: !Double,
                     serviceRate :: !Double,
                     numServers  :: !Int
                   }

-- Events are tagged as being either arrivals or departures.
data EventType = Arrival | Departure
                 deriving Eq

-- Update a markovian queue's state
update :: PrimMonad m =>
          MarkovQueueState  ->
          MarkovQueue       ->
          Gen (PrimState m) ->
          m MarkovQueueState
update !(MarkovQueueState t n) (MarkovQueue lambda mu c) rng = do

  -- Departure rate
  let mu' = if n >= c
            then fromIntegral c * mu
            else fromIntegral n * mu

  -- Generate the event time
  e <- fmap (/(lambda + mu')) $ exponential rng
  let t' = t + e

  -- Generate the event type
  u <- uniform rng
  let eventType = if u < lambda/(lambda+mu')
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

