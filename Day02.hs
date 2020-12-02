module Day02
  ( solveDay02
  )
where

import Control.DeepSeq (NFData(..))
import Data.Char
import Text.ParserCombinators.ReadP

import Util (Solver(..), runSolver)


data Policy = Policy Int Int Char

instance NFData Policy where
  rnf (Policy a b c) = a `seq` b `seq` c `seq` ()


parseLine :: String -> (Policy, String)
parseLine s = head $ readP_to_S parser s
 where
  parser = do
    minC <- read <$> munch1 isDigit
    char '-'
    maxC <- read <$> munch1 isDigit
    skipSpaces
    chr <- get
    string ": "
    return $ Policy minC maxC chr


part1Valid :: (Policy, String) -> Bool
part1Valid (Policy minC maxC pChr, str) = c >= minC && c <= maxC
  where c = length $ filter (== pChr) str


part2Valid :: (Policy, String) -> Bool
part2Valid (Policy pos1 pos2 pChr, str) =
  (str !! (pos1 - 1) == pChr) /= (str !! (pos2 - 1) == pChr)


solveDay02 :: IO ()
solveDay02 = runSolver Solver
  { inputFile  = "inputs/02.txt"
  , parseInput = map parseLine . lines
  , part1      = Just . length . filter part1Valid
  , part2      = Just . length . filter part2Valid
  }

