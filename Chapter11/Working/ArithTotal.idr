import InfIO
import Data.Primitives.Views
import System

%default total

quiz : Stream Int -> (score : Nat) -> InfIO
quiz (n1 :: n2 :: nums) score = do
  putStrLn $ "Score so far: " ++ show score
  putStr $ show n1 ++ " * " ++ show n2 ++ "? "
  answer <- getLine
  if cast answer == n1 * n2
     then do putStrLn "Correct!"
             quiz nums (score + 1)
     else do putStrLn $ "Wrong, the answer is " ++ show (n1 * n2)
             quiz nums score

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound n with (divides n 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do
  seed <- time
  run forever (quiz (arithInputs (fromInteger seed)) 0)
