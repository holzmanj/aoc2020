module Day05
  ( day05Solver
  )
where

import Control.Applicative
import Data.List

import Util


seatID :: String -> Int
seatID = foldl' (\acc x -> acc * 2 + fromEnum (x `elem` "BR")) 0


findSeat :: [Int] -> Int
findSeat = head . liftA2 (\\) (\l -> [minimum l .. maximum l]) id


day05Solver = Solver
  { inputFile  = "inputs/05.txt"
  , parseInput = map seatID . lines
  , part1      = maximum
  , part2      = findSeat
  }


main :: IO ()
main = runSolver day05Solver
