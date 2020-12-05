module Day05
  ( solveDay05
  )
where

import Control.Applicative
import Data.List

import Util


seatID :: String -> Int
seatID = foldl' (\acc x -> acc * 2 + fromEnum (x `elem` "BR")) 0


findSeat :: [Int] -> Int
findSeat = head . liftA2 (\\) (\l -> [minimum l .. maximum l]) id


solveDay05 :: IO ()
solveDay05 = runSolver Solver
  { inputFile  = "inputs/05.txt"
  , parseInput = map seatID . lines
  , part1      = Just . maximum
  , part2      = Just . findSeat
  }
