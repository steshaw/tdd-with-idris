module Main

import RunIO -- (forever)
import Commands

%default total

FilePath : Type
FilePath = String

data ShellCommand
  = Cat FilePath
  | Copy FilePath FilePath
  | Exit
  | Empty
  | BadCommand String

readShellCommand : (prompt : String) -> Command ShellCommand
readShellCommand prompt = do
  putStr prompt
  s <- getLine
  let ws = words s
  case ws of
       ["cat", f] => pure $ Cat f
       ["copy", src, dest] => pure $ Copy src dest
       ["exit"] => pure Exit
       []       => pure Empty
       _        => pure $ BadCommand s

cat : FilePath -> Command ()
cat f = do
  r <- readFile f
  case r of
       (Left l)         =>  do putStrLn $ "File error: " ++ show l
       (Right contents) => putStrLn contents

writeContentsToDest : (dest: FilePath) -> (contents : String) -> Command ()
writeContentsToDest dest contents = do
  r <- writeFile dest contents
  case r of
       Left l   => putStrLn $ "Unable to write to '" ++ dest ++ "': " ++ show l
       Right () => pure ()

copy : FilePath -> FilePath -> Command ()
copy src dest = do
  r <- readFile src
  case r of
       (Left l)         => do putStrLn $ "Unable to read '" ++ src ++ "': " ++ show l
       (Right contents) => writeContentsToDest dest contents

shell : ConsoleIO ()
shell = do
  input <- readShellCommand "shell> "
  case input of
       Cat f         => do cat f
                           shell
       Copy src dest => do copy src dest
                           shell
       Exit          => Quit ()
       Empty         => shell
       BadCommand s  => do putStrLn $ "Invalid command '" ++ s ++ "'"
                           shell
       _             => do putStrLn "not implemented"
                           Quit ()

partial
main : IO ()
main = do
  run forever shell
  pure ()
