module Main where

-- Imports
import Lib
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

  -- Set up M/M/1 queue with a given arrival and service rate
  let queue = MarkovQueue 1.0 1.4

  -- Start the queue empty
  let initialState = MarkovQueueState 0.0 0

  -- Evolve until 10000 events have occurred
  _ <- run 0 10000 initialState queue rng

  return ()

