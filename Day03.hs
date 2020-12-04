module Day03
  ( solveDay03
  )
where

import Control.Applicative

import Util

-- spaces encoded as 0, trees encoded as 1
parseLine :: String -> [Int]
parseLine = cycle . map (fromEnum . (== '#'))


-- count trees on slope of grid
trees :: (Int, Int) -> [[Int]] -> Int
trees (x, y) s = liftA2 (+) trees' (head . head) (drop x <$> drop y s)
  where trees' m = if null $ drop y m then 0 else trees (x, y) m


slopes :: [(Int, Int)]
slopes = [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]


solveDay03 :: IO ()
solveDay03 = runSolver Solver
  { inputFile  = "inputs/03.txt"
  , parseInput = map parseLine . lines
  , part1      = Just . trees (head slopes)
  , part2      = Just . product . ((<$> slopes) . flip trees)
  }
