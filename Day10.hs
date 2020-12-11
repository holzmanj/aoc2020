module Day10
  ( day10Solver
  )
where

import Control.Applicative (liftA2)
import Data.List
import qualified Data.Map as M

import Util


joltDiffs :: [Int] -> Int
joltDiffs x = liftA2 (*) ((+ 1) . count 3) (count 1) (zipWith (-) (tail x) x)
  where count n = length . filter (== n)


subseqs :: [Int] -> Int
subseqs = subseqIter M.empty . tail . reverse . map (take 4) . tails
 where
  subseqIter mem ((a : bs) : rest) =
    let
      subseqCount = if null bs
        then 1
        else sum . map (mem M.!) . filter ((<= 3) . subtract a) $ bs
      mem' = M.insert a subseqCount mem
    in if null rest then mem' M.! 0 else subseqIter mem' rest


day10Solver = Solver
  { inputFile  = "inputs/10.txt"
  , parseInput = (0 :) . sort . map read . lines
  , part1      = joltDiffs
  , part2      = subseqs
  }


main :: IO ()
main = runSolver day10Solver
