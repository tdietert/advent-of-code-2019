module Day2

import Control.ST
import Control.Monad.Identity
import Prelude.List as List
import Data.SortedMap as SMap
import Data.Vect as V
import Prelude.Traversable
import Debug.Trace

export
data Day2 = D2

Show Day2 where
  show D2 = "D2"

data Op = Add | Mult | Halt

Show Op where
  show Add = "Add"
  show Mult = "Mult"
  show Halt = "Halt"

record Intcode where
  constructor IC
  op : Op
  reg1 : Int
  reg2 : Int
  reg3 : Int

Show Intcode where
  show (IC op r1 r2 r3) =
    unwords ["IC", show op, show r1, show r2, show r3]

Tape : Type
Tape = SortedMap Int Int

||| Parsing Input

maybeToList : Maybe a -> List a
maybeToList Nothing = Nil
maybeToList (Just a) = [a]

mapMaybe : List (Maybe a) -> List a
mapMaybe Nil = Nil
mapMaybe (Nothing :: rest) = mapMaybe rest
mapMaybe (Just a :: rest) = a :: mapMaybe rest

parseInt : String -> Maybe Int
parseInt str =
  if all isDigit (unpack str)
     then Just (cast str)
     else Nothing

parseInput : String -> Tape
parseInput s = tape
  where
    ints : List Int
    ints = mapMaybe parseInt (Prelude.Strings.split (== ',') s)

    tape : Tape
    tape = SMap.fromList (zip keys ints)
      where
        keys = [0.. (cast (List.length ints) - 1)]

||| Problem Solving

record IntcodeState where
  constructor ICState
  pos : Int
  tape : Tape

lookupReg : Int -> Tape -> Either String Int
lookupReg n tape =
  case lookup n tape of
    Nothing => Left "Failed to lookup register"
    Just v => Right v

updateReg : Int -> Int -> Tape -> Tape
updateReg r v tape = insert r v tape

intToOp : Int -> Either String Op
intToOp op =
  case op of
     1 => Right Add
     2 => Right Mult
     99 => Right Halt
     _ => Left "Invalid op code"

readIntcode : (ics : Var)
            -> STrans (Either String) Intcode
                 [ics ::: State IntcodeState]
                 (const [ics ::: State IntcodeState])
readIntcode ics = do
  ICState pos tape <- read ics
  let keys = V.fromList (zipWith (+) (replicate 4 pos) [0..3])
  (op' :: src1 :: src2 :: dest :: Nil) <- lift (traverse (flip lookupReg tape) keys)
  write ics (ICState (pos + 4) tape)
  op <- lift (intToOp op')
  pure (IC op src1 src2 dest)

evalIntcode' : (Int -> Int -> Int)
            -> Int -> Int -> Int
            -> Tape
            -> Either String Tape
evalIntcode' f r1 r2 r3 tape = do
  vr1 <- lookupReg r1 tape
  vr2 <- lookupReg r2 tape
  pure (updateReg r3 (f vr1 vr2) tape)

data ICResult = ICContinue | ICHalt

evalIntcode : (ics : Var)
           -> Intcode
           -> STrans (Either String) ICResult
                [ics ::: State IntcodeState]
                (const [ics ::: State IntcodeState])
evalIntcode ics (IC op src1 src2 dest) = do

    ICState pos tape <- read ics

    case op of
      Add => do
        newTape <- lift (evalIntcode' (+) src1 src2 dest tape)
        write ics (ICState pos newTape)
        pure ICContinue
      Mult => do
        newTape <- lift (evalIntcode' (*) src1 src2 dest tape)
        write ics (ICState pos newTape)
        pure ICContinue
      Halt => pure ICHalt

runIntcodes : (ics : Var)
           -> STrans (Either String) ()
                [ics ::: State IntcodeState]
                (const [ics ::: State IntcodeState])
runIntcodes ics = do
  ic <- readIntcode ics
  res <- evalIntcode ics ic
  case res of
    ICContinue => runIntcodes ics
    ICHalt => pure ()

initializeTape : (Int, Int) -> Tape -> Tape
initializeTape (one,two) tape =
  updateReg 2 two (updateReg 1 one tape)

part1' : (Int, Int) -> Tape -> Either String Tape
part1' init tape = do
  run $ do
    let initTape = initializeTape init tape
    var <- new (ICState 0 initTape)
    runIntcodes var
    ICState _ resTape <- read var
    delete var
    pure resTape

part1 : Tape -> Either String Tape
part1 = part1' (12,2)

||| Find the two ints with which to intialize address 1 and 2 to such that the
||| result of running the Intcode program (the value in address 0) is 19690720;
||| Then, given the (x,y) pair, return 100 * x + y.
part2 : Tape -> Either String Int
part2 tape = tryPairs pairs
  where
    pairs : List (Int, Int)
    pairs = [(x,y) | x <- [0..99], y <- [0..99]]

    tryPair : (Int, Int) -> Either String (Maybe Int)
    tryPair pair@(x,y) = do
      resTape <- part1' pair tape
      resAtZero <- lookupReg 0 resTape
      if resAtZero == 19690720
         then pure $ Just (100 * x + y)
         else pure Nothing

    tryPairs : List (Int,Int) -> Either String Int
    tryPairs Nil = Left "Solution not found"
    tryPairs (pair::rest) = do
      res <- tryPair pair
      case res of
        Nothing => tryPairs rest
        Just n => pure n

||| Running part1 and part2

runDay2 : Show a => String -> (Tape -> Either String a) -> IO ()
runDay2 strInput runPart = do
  case runPart (parseInput strInput) of
    Left err => putStrLn err
    Right smap => print smap

runDay2Part1 : String -> IO ()
runDay2Part1 strInput =
  runDay2 strInput part1

runDay2Part2 : String -> IO ()
runDay2Part2 strInput =
  runDay2 strInput part2
