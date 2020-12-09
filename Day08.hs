module Day08 where

import Data.Set (Set, delete, empty, insert, member)
import Data.Vector (Vector, (!), (//), fromList)

import Util


data State = State
    { pc   :: Int
    , acc  :: Int
    , prog :: Vector Inst
    , seen :: Set Int
    }
type Inst = (Op, Int)
data Op = Nop | Acc | Jmp deriving (Eq)


flipOp :: State -> State
flipOp s =
  let
    (op, val) = prog s ! pc s
    newOp     = case op of
      Acc -> Acc
      Jmp -> Nop
      Nop -> Jmp
    seen' = delete (pc s) (seen s)
  in s { prog = prog s // [(pc s, (newOp, val))], seen = seen' }


step :: State -> State
step s =
  let
    (op, val) = prog s ! pc s
    seen'     = insert (pc s) (seen s)
  in (case op of
       Nop -> s { pc = pc s + 1 }
       Acc -> s { pc = pc s + 1, acc = acc s + val }
       Jmp -> s { pc = pc s + val }
     )
    { seen = seen'
    }


runUntilLoop :: State -> State
runUntilLoop st =
  let (inst, val) = prog st ! pc st
  in
    if member (pc st) (seen st) || pc st == length (prog st)
      then st
      else runUntilLoop (step st)


fixBadOp :: State -> State
fixBadOp st = if fst (prog st ! pc st) == Acc
  then fixBadOp (step st)
  else
    let branched = runUntilLoop $ flipOp st
    in
      if pc branched >= length (prog branched)
        then branched
        else fixBadOp $ step st { seen = seen branched }


parseInst :: String -> Inst
parseInst = uncurry parseInst' . splitAt 3
 where
  parseInst' a b =
    let val = read (filter (`notElem` " +") b) :: Int
    in
      case a of
        "nop" -> (Nop, val)
        "acc" -> (Acc, val)
        "jmp" -> (Jmp, val)
        _     -> error $ "Unrecognized instruction: " ++ show a


initState :: [Inst] -> State
initState p = State { pc = 0, acc = 0, prog = fromList p, seen = empty }


solveDay08 :: IO ()
solveDay08 = runSolver Solver
  { inputFile  = "inputs/08.txt"
  , parseInput = initState . map parseInst . lines
  , part1      = Just . acc . runUntilLoop
  , part2      = Just . acc . fixBadOp
  }


