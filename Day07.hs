
import Data.Char
import Data.List hiding (transpose)
import Data.List.Split
import Text.ParserCombinators.ReadP

import Util


type Vertex = String
type Edge = (Vertex, Vertex, Int)
type Graph = [Edge]


-- generate subgraph for one bag
readBag :: String -> Graph
readBag s = parseEdges y
 where
  [x, y]     = splitOn " bags contain " s
  parseEdges = fst . last . readP_to_S (many parseEdge)
  parseEdge  = do
    count <- read <$> munch1 isDigit
    skipSpaces
    style <- munch1 isAlpha
    skipSpaces
    color <- munch1 isAlpha
    string " bag" >> choice (string <$> [", ", "s, ", ".", "s."])
    return (x, unwords [style, color], count)


transpose :: Graph -> Graph
transpose = map (\(u, v, w) -> (v, u, w))


reachable :: Graph -> Vertex -> [Vertex]
reachable g v = nub $ vs ++ concatMap (reachable g) vs
  where vs = (\(_, w, _) -> w) <$> filter (\(w, _, _) -> v == w) g


countNested :: Graph -> Vertex -> Int
countNested g = subtract 1 . count
 where
  from vtx (x, _, _) = vtx == x
  count vtx = 1 + sum (count' <$> filter (from vtx) g)
  count' (_, vtx, wgt) = wgt * count vtx


solveDay07 :: IO ()
solveDay07 = runSolver Solver
  { inputFile  = "inputs/07.txt"
  , parseInput = concatMap readBag . lines
  , part1      = Just . length . flip reachable "shiny gold" . transpose
  , part2      = Just . flip countNested "shiny gold"
  }
