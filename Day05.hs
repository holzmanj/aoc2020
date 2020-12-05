module Day05
  ( solveDay05
  )
where

import Control.Applicative
import Data.List

import Util


seatID :: String -> Int
seatID s = toNum row * 8 + toNum col
 where
  toNum      = foldl' (\acc x -> acc * 2 + fromEnum (x `elem` "BR")) 0
  (row, col) = splitAt 7 s


findSeat :: [Int] -> Int
findSeat ss = (snd . head) $ filter (liftA2 (/=) fst snd) (zip ss [head ss ..])


solveDay05 :: IO ()
solveDay05 = runSolver Solver
  { inputFile  = "inputs/05.txt"
  , parseInput = map seatID . lines
  , part1      = Just . maximum
  , part2      = Just . findSeat . sort
  }
