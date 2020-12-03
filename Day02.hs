module Day02
  ( solveDay02
  )
where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Char
import Data.Function
import Text.ParserCombinators.ReadP

import Util (Solver(..), runSolver)


data Policy = Policy Int Int Char

instance NFData Policy where
  rnf (Policy a b c) = a `seq` b `seq` c `seq` ()


parseLine :: String -> (Policy, String)
parseLine = head . readP_to_S parser
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
part1Valid (Policy minC maxC pChr, str) = liftA2 (&&) (>= minC) (<= maxC) cnt
  where cnt = length $ filter (== pChr) str


part2Valid :: (Policy, String) -> Bool
part2Valid (Policy pos1 pos2 pChr, str) = on (/=) pChrAt pos1 pos2
  where pChrAt = (== pChr) . (str !!) . subtract 1


solveDay02 :: IO ()
solveDay02 = runSolver Solver
  { inputFile  = "inputs/02.txt"
  , parseInput = map parseLine . lines
  , part1      = Just . length . filter part1Valid
  , part2      = Just . length . filter part2Valid
  }

