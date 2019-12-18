module Day3

||| Datastructures

data Dir = U | D | L | R

Show Dir where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

data WireSegment = WireSeg Dir Nat

Show WireSegment where
  show (WireSeg dir nat) = "WireSegment " ++ show dir ++ " " ++ show nat

Wire : Type
Wire = List WireSegment

||| Input Parsing

parseNat : String -> Either String Nat
parseNat str =
  if all isDigit (unpack str)
     then Right (cast str)
     else Left "failed to parse nat"

parseDir : String -> Either String Dir
parseDir "U" = Right U
parseDir "D" = Right D
parseDir "L" = Right L
parseDir "R" = Right R
parseDir _ = Left "Failed to parse Dir "

parseWireSegment : String -> Either String WireSegment
parseWireSegment str =
  case unpack str of
    Nil => Left "failed to parse wire"
    (dir::nat) => WireSeg <$> parseDir (singleton dir) <*> parseNat (pack nat)

parseWire : String -> Either String Wire
parseWire = traverse parseWireSegment . split ((==) ',')

parseWires : String -> Either String (List Wire)
parseWires = traverse parseWire . unlines

||| Solution

help : Type
help = ?help
