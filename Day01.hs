module Day01
  ( solveDay01
  )
where

import Data.Foldable
import Data.List

import Util (Solver(..), runSolver)


-- find two numbers whose sum is 2020, multiply them
solvePart1 :: [Int] -> Maybe Int
solvePart1 = (\nums -> solveIter nums (reverse nums)) . sort
 where
  solveIter (x : xs) (y : ys)
    | x == y = Nothing
    | otherwise = case compare (x + y) 2020 of
      EQ -> Just $ x * y
      LT -> solveIter xs (y : ys)
      GT -> solveIter (x : xs) ys


-- find three numbers whose sum is 2020, multiply them
solvePart2 :: [Int] -> Maybe Int
solvePart2 =
  asum . map (\(x : xs) -> solveIter x xs (reverse xs)) . (tails . sort)
 where
  solveIter x (y : ys) (z : zs)
    | y == z = Nothing
    | otherwise = case compare (x + y + z) 2020 of
      EQ -> Just $ x * y * z
      LT -> solveIter x ys (z : zs)
      GT -> solveIter x (y : ys) zs


solveDay01 :: IO ()
solveDay01 = runSolver Solver
  { inputFile  = "inputs/01.txt"
  , parseInput = map read . lines
  , part1      = solvePart1
  , part2      = solvePart2
  }

