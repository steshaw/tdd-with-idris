module Commands

import RunIO

%default total

public export
data Command : Type -> Type where
  PutStr    : String -> Command ()
  GetLine   : Command String
  ReadFile  : String -> Command (Either FileError String)
  WriteFile : String -> String -> Command (Either FileError ())
  Pure      : (val : ty) -> Command ty
  Bind      : (cmd : Command a) -> (k : (a -> Command b)) -> Command b

namespace CommandDo
  public export
  (>>=) : (cmd : Command a) -> (k : (a -> Command b)) -> Command b
  (>>=) = Bind

public export
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  public export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

export
runCommand : Command a -> IO a
runCommand (PutStr s) = putStr s
runCommand GetLine = getLine
runCommand (ReadFile f) = readFile f
runCommand (WriteFile f c) = writeFile f c
runCommand (Pure val) = pure val
runCommand (Bind cmd k) = do result <- runCommand cmd
                             runCommand (k result)

export
run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val)            = pure (Just val)
run Dry p                      = pure Nothing
run (More fuel) (Do command k) = do result <- runCommand command
                                    run fuel (k result)

export
putStr : String -> Command ()
putStr = PutStr

export
putStrLn : String -> Command ()
putStrLn s = PutStr $ s ++ "\n"

export
getLine : Command String
getLine = GetLine

export
readFile : String -> Command (Either FileError String)
readFile = ReadFile

export
writeFile : String -> String -> Command (Either FileError ())
writeFile = WriteFile

export
pure : (val : ty) -> Command ty
pure = Pure
