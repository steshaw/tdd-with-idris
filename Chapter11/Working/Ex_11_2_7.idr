import InfIO

%default total

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  line <- getLine
  putStrLn $ action line
  totalREPL prompt action
