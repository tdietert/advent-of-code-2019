module Main

import Day1 

parseNat : String -> Maybe Nat
parseNat str =
  if all isDigit (unpack str) 
     then Just (cast str)
     else Nothing

main : IO ()
main = do
  args <- getArgs
  case args of
    (_::fp::xs) => do
      print args
      if isCons xs 
         then putStrLn "Expecting a single file as argument"
         else do
           eInputData <- readFile fp 
           case eInputData of
              Left err => print err
              Right input => do
                let inputLines = lines input
                print (Day1.part2 (mapMaybe parseNat inputLines))  
    _ => putStrLn "Expecting a single file as argument"
