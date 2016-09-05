module ArithCmdDo

import ArithStreams
import RunIO
import System
import Commands

%default total

data Input
  = Answer Int
  | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do
  putStr prompt
  answer <- getLine
  if toLower answer == "quit"
     then Pure QuitCmd
     else Pure $ Answer (cast answer)

mutual
  correct : (nums : Stream Int) -> (score : Nat) -> (numQ : Nat) -> ConsoleIO (Nat, Nat)
  correct nums score numQ = do
    putStrLn "Correct!"
    quiz nums (score + 1) (numQ + 1)

  incorrect : (answer : Int) -> (nums : Stream Int) -> (score : Nat) -> (numQ : Nat) -> ConsoleIO (Nat, Nat)
  incorrect answer nums score numQ = do
    putStrLn $ "Wrong, the answer is " ++ show answer
    quiz nums score (numQ + 1)

  quiz : Stream Int -> (score : Nat) -> (numQ : Nat) -> ConsoleIO (Nat, Nat)
  quiz (n1 :: n2 :: nums) score numQ = do
    putStrLn $ "Score so far: " ++ show score ++ " / " ++ show numQ
    input <- readInput $ show n1 ++ " * " ++ show n2 ++ "? "
    case input of
         Answer a => if a == n1 * n2
                        then correct nums score numQ
                        else incorrect (n1 * n2) nums score numQ
         QuitCmd  => Quit (score, numQ)

partial
main : IO ()
main = do
  seed <- time
  Just (score, numQ) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
    | Nothing => putStrLn "Ran out of fuel"
  putStrLn $ "Final score: " ++ show score ++ " / " ++ show numQ
