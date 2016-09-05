module ArithCmd

import ArithStreams
import RunIO
import System

%default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) :  Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr s) = putStr s
runCommand GetLine = getLine

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val)            = pure (Just val)
run Dry p                      = pure Nothing
run (More fuel) (Do command k) = do result <- runCommand command
                                    run fuel (k result)

putStr : String -> Command ()
putStr s = PutStr s

putStrLn : String -> Command ()
putStrLn s = PutStr $ s ++ "\n"

getLine : Command String
getLine = GetLine

mutual
  correct : (nums : Stream Int) -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do
    putStrLn "Correct!"
    quiz nums (score + 1)

  incorrect : (answer : Int) -> (nums : Stream Int) -> (score : Nat) -> ConsoleIO Nat
  incorrect answer nums score = do
    putStrLn $ "Wrong, the answer is " ++ show answer
    quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (n1 :: n2 :: nums) score = do
    putStrLn $ "Score so far: " ++ show score
    putStr $ show n1 ++ " * " ++ show n2 ++ "? "
    answer <- getLine
    if answer == "" 
       then Quit score
       else if cast answer == n1 * n2
               then do correct nums score
               else do incorrect (n1 * n2) nums score

partial
main : IO ()
main = do
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
    | Nothing => putStrLn "Ran out of fuel"
  putStrLn $ "Final score: " ++ show score
