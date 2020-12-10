module Day09 where

import Control.Applicative
import Data.Vector (Vector, (!), fromList, modify, slice)
import qualified Data.Vector.Algorithms.Intro as V

import Util


has2Sum :: Int -> Vector Int -> Bool
has2Sum n vec = checkIter 0 . subtract 1 . length $ vec
 where
  srtd = modify V.sort vec
  checkIter i j = (i /= j) && case compare (srtd ! i + srtd ! j) n of
    EQ -> True
    LT -> checkIter (i + 1) j
    GT -> checkIter i (j - 1)


findBadNum :: Vector Int -> Int
findBadNum vec = findBadIter 25
 where
  findBadIter idx =
    let
      preamble = slice (idx - 25) 25 vec
      n        = vec ! idx
    in if has2Sum n preamble then findBadIter (idx + 1) else n


findSumSeq :: Int -> Vector Int -> Int
findSumSeq target vec = sumSeqIter 0 1 (vec ! 0)
 where
  addMinMax = liftA2 (+) minimum maximum
  sumSeqIter i len part =
    let
      pushSum = part + (vec ! (i + len))
      popSum  = part - (vec ! i)
    in case compare pushSum target of
      EQ -> addMinMax (slice i len vec)
      GT -> sumSeqIter (i + 1) (len - 1) popSum
      LT -> sumSeqIter i (len + 1) pushSum


day09Solver = Solver
  { inputFile  = "inputs/09.txt"
  , parseInput = fromList . map (read :: String -> Int) . lines
  , part1      = findBadNum
  , part2      = \v -> findSumSeq (findBadNum v) v
  }


main :: IO ()
main = runSolver day09Solver
