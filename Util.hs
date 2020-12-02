module Util where

import Control.DeepSeq
import Data.Functor
import Text.Printf
import System.CPUTime


data Solver a b c = Solver
    { inputFile :: String
    , parseInput :: String -> a
    , part1 :: a -> Maybe b
    , part2 :: a -> Maybe c
    }


benchmark :: (Show b, NFData b) => (a -> Maybe b) -> a -> IO ()
benchmark f x = do
  t0 <- getCPUTime
  let res = f x
  t1 <- res `deepseq` getCPUTime
  let diff = fromIntegral (t1 - t0) / (10 ^ 12) :: Float
  case res of
    Nothing -> putStrLn "\tFailed to find a solution"
    Just s  -> putStrLn $ "\tSolution: " ++ show s
  printf "\tTime elapsed: %.06f seconds\n" diff


runSolver
  :: (NFData a, NFData b, NFData c, Show b, Show c) => Solver a b c -> IO ()
runSolver solver = do
  input <- force . parseInput solver <$> readFile (inputFile solver)
  putStrLn "PART 1" >> benchmark (part1 solver) input
  putStrLn "PART 2" >> benchmark (part2 solver) input
