import Data.Primitives.Views

total
randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

total
arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound n with (divides n 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (n1 :: n2 :: nums) score = do
  putStrLn $ "Score so far: " ++ show score
  putStr $ show n1 ++ " * " ++ show n2 ++ "? "
  answer <- getLine
  if cast answer == n1 * n2
     then do putStrLn "Correct!"
             quiz nums (score + 1)
     else do putStrLn $ "Wrong, the answer is " ++ show (n1 * n2)
             quiz nums score
