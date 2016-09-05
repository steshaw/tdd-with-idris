module ArithCmdDo

import ArithStreams
import RunIO
import System

%default total

data Command : Type -> Type where
  PutStr  : String -> Command ()
  GetLine : Command String
  Pure    : (val : ty) -> Command ty
  Bind    : (cmd : Command a) -> (k : (a -> Command b)) -> Command b

namespace CommandDo
  (>>=) : (cmd : Command a) -> (k : (a -> Command b)) -> Command b
  (>>=) = Bind

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr s) = putStr s
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind cmd k) = do result <- runCommand cmd
                             runCommand (k result)

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
    input <- readInput $ show n1 ++ " * " ++ show n2 ++ "? "
    case input of
         Answer a => if a == n1 * n2
                        then correct nums score
                        else incorrect (n1 * n2) nums score
         QuitCmd  => Quit score

partial
main : IO ()
main = do
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
    | Nothing => putStrLn "Ran out of fuel"
  putStrLn $ "Final score: " ++ show score
