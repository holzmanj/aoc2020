module Util where

import Criterion.Main


data Solver a b c = Solver
    { inputFile :: String
    , parseInput :: String -> a
    , part1 :: a -> b
    , part2 :: a -> c
    }


runSolver :: (Show b, Show c) => Solver a b c -> IO ()
runSolver solver = do
  input <- parseInput solver <$> readFile (inputFile solver)
  putStrLn $ "Part 1 (silver):  " ++ show (part1 solver input)
  putStrLn $ "Part 2 (gold):    " ++ show (part2 solver input)


benchmark :: Solver a b c -> IO ()
benchmark solver = do
  input <- parseInput solver <$> readFile (inputFile solver)
  defaultMain
    [ bench "part 1 (silver)" $ whnf (part1 solver) input
    , bench "part 2 (gold)" $ whnf (part2 solver) input
    ]

