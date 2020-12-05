module Day04
  ( solveDay04
  )
where

import Prelude hiding (splitAt, lookup)
import Data.List.Split
import Data.Map.Strict (Map, fromList, member, lookup)
import Text.Regex.TDFA

import Util

type Passport = Map String String

parse :: String -> [Passport]
parse = map (fromList . (map (toTuple . splitOn ":") . words)) . splitOn "\n\n"
  where toTuple [a, b] = (a, b)


fieldPatterns :: [(String, String)]
fieldPatterns =
  [ ("byr", "^(19[2-8][0-9]|199[0-9]|200[0-2])$")
  , ("iyr", "^(201[0-9]|2020)$")
  , ("eyr", "^(202[0-9]|2030)$")
  , ("hgt", "^((1[5-8][0-9]|19[0-3])cm)|((59|6[0-9]|7[0-6])in)$")
  , ("hcl", "^#[0-9a-f]{6}$")
  , ("ecl", "^(amb|blu|brn|gry|grn|hzl|oth)$")
  , ("pid", "^[0-9]{9}$")
  ]


hasFields :: Passport -> Bool
hasFields = flip all (map fst fieldPatterns) . flip member


validFields :: Passport -> Bool
validFields = flip all fieldPatterns . validField
 where
  validField :: Passport -> (String, String) -> Bool
  validField pass (fld, pat) =
    let val = lookup fld pass in maybe False (=~ pat) val


solveDay04 :: IO ()
solveDay04 = runSolver Solver
  { inputFile  = "inputs/04.txt"
  , parseInput = parse
  , part1      = Just . length . filter hasFields
  , part2      = Just . length . filter validFields
  }

