module Util where

import Criterion.Main


data Solver a b c = Solver
    { inputFile :: String
    , parseInput :: String -> a
    , part1 :: a -> Maybe b
    , part2 :: a -> Maybe c
    }


runSolver :: (Show b, Show c) => Solver a b c -> IO ()
runSolver solver = do
  input <- parseInput solver <$> readFile (inputFile solver)
  putStrLn "-- SOLUTIONS --"
  putStrLn $ "Part 1 (silver):  " ++ maybeShow (part1 solver input)
  putStrLn $ "Part 2 (gold):    " ++ maybeShow (part2 solver input)
  putStrLn "\n\n-- PERFORMANCE --"
  defaultMain
    [ bench "part 1 (silver)" $ whnf (part1 solver) input
    , bench "part 2 (gold)" $ whnf (part2 solver) input
    ]
  where maybeShow x = maybe "Faild to find a solution" show x
