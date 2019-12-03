module Day1 

export
data Day1 = D1

Show Day1 where
  show D1 = "D1"

export
parseDay1 : String -> Maybe Day1
parseDay1 str =
  if str == show D1
     then Just D1 
     else Nothing

||| It'd be really great if we could return a proof that the output is smaller
||| than the input, but I don't yet know how to do that. Such a proof would be 
||| needed to prove the caller functions of 'fuel' are total; i.e. to prove that
||| recursing on the result of 'fuel' is recursing on a smaller value than the
||| input provided to 'fuel'.
fuel : Nat -> Nat
fuel n =
  -- 3 or (S (S (S Z)) is never Z
  let m = divNatNZ n 3 SIsNotZ in
  case m of
       S (S k) => k -- subtracting 2
       _ => Z -- all other cases return 0

export
part1 : List Nat -> Nat
part1 = sum . map fuel

export 
part2 : List Nat -> Nat 
part2 = sum . map fuelAndFuelFuel
  where 
    fuelFuel : Nat -> Nat -> Nat
    fuelFuel acc Z = acc
    fuelFuel acc n =
      case n of 
           Z => acc
           _ => let n' = fuel n in
                    -- A smarter person could prove totality here
                    assert_total (fuelFuel (acc + n') n')
   
    fuelAndFuelFuel : Nat -> Nat
    fuelAndFuelFuel m = 
      let f = fuel m in
          -- A smarter person could prove totality here
          f + assert_total (fuelFuel 0 f)

