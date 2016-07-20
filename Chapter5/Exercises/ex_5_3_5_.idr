import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if line == ""
    then pure []
    else do
      lines <- readToBlank
      pure $ line :: lines

readAndSave : IO ()
readAndSave = do
  putStrLn "Enter strings:"
  lines <- readToBlank
  putStr "Enter filename: "
  filename <- getLine
  Right () <- writeFile filename (unlines lines) | Left fileError =>
    putStrLn ("File error: " ++ show fileError)
  putStrLn "Written."

fReadLines : File -> IO (n ** Vect n String)
fReadLines file = do
  False <- fEOF file | True => pure (_ ** [])
  Right line <- fGetLine file | Left fileError => do
    putStrLn $ "File error: " ++ show fileError
    pure (_ ** [])
  (n ** lines) <- fReadLines file
  pure $ (S n ** line :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read | Left fileError => do
    putStrLn $ "File error: " ++ show fileError
    pure (_ ** [])
  lines <- fReadLines file
  closeFile file
  pure lines
