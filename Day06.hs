module Day06
  ( solveDay06
  )
where

import Data.List
import Data.List.Split

import Util


solve :: (String -> String -> String) -> [[String]] -> Int
solve f = length . concatMap (foldr1 f)


solveDay06 :: IO ()
solveDay06 = runSolver Solver
  { inputFile  = "inputs/06.txt"
  , parseInput = map lines . splitOn "\n\n"
  , part1      = Just . solve union
  , part2      = Just . solve intersect
  }

