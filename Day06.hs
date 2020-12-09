module Day06
  ( day06Solver
  )
where

import Data.List
import Data.List.Split

import Util


solve :: (String -> String -> String) -> [[String]] -> Int
solve f = length . concatMap (foldr1 f)


day06Solver = Solver
  { inputFile  = "inputs/06.txt"
  , parseInput = map lines . splitOn "\n\n"
  , part1      = solve union
  , part2      = solve intersect
  }


main :: IO ()
main = runSolver day06Solver
