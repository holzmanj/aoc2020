module Day06
  ( solveDay06
  )
where

import Data.List
import Data.List.Split

import Util


solveDay06 :: IO ()
solveDay06 = runSolver Solver
  { inputFile  = "inputs/06.txt"
  , parseInput = splitOn "\n\n"
  , part1      = Just . sum . map (length . nub . filter (/= '\n'))
  , part2      = Just . sum . map (length . foldr1 intersect . lines)
  }

