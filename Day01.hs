module Day01
  ( day01Solver
  )
where

import Data.Foldable
import Data.List
import Data.Maybe

import Util


-- find two numbers whose sum is 2020, multiply them
solvePart1 :: [Int] -> Int
solvePart1 = (\nums -> solveIter nums (reverse nums)) . sort
 where
  solveIter (x : xs) (y : ys)
    | x == y = error "No solution!"
    | otherwise = case compare (x + y) 2020 of
      EQ -> x * y
      LT -> solveIter xs (y : ys)
      GT -> solveIter (x : xs) ys


-- find three numbers whose sum is 2020, multiply them
solvePart2 :: [Int] -> Int
solvePart2 =
  first . map (\(x : xs) -> solveIter x xs (reverse xs)) . (tails . sort)
 where
  first = fromMaybe (error "No solution!") . asum
  solveIter x (y : ys) (z : zs)
    | y == z = Nothing
    | otherwise = case compare (x + y + z) 2020 of
      EQ -> Just $ x * y * z
      LT -> solveIter x ys (z : zs)
      GT -> solveIter x (y : ys) zs


day01Solver = Solver
  { inputFile  = "inputs/01.txt"
  , parseInput = map read . lines
  , part1      = solvePart1
  , part2      = solvePart2
  }

main :: IO ()
main = runSolver day01Solver
