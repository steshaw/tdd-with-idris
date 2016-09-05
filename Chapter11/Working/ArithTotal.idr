import InfIO
import Data.Primitives.Views
import System
import ArithStreams

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

partial
main : IO ()
main = do
  seed <- time
  run forever (quiz (arithInputs (fromInteger seed)) 0)
