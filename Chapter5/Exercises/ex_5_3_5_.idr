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
  ff <- writeFile filename (unlines lines)
  case ff of
    Left fileError => putStrLn $ "File error: " ++ show fileError
    Right () => putStrLn "Written."

fReadToEOF : File -> IO (n ** Vect n String)
fReadToEOF file = do
  eof <- fEOF file
  if eof
  then pure (_ ** [])
  else do
    es <- fGetLine file
    case es of
      (Left fileError) => do
        putStrLn $ "File error: " ++ show fileError
        pure (_ ** [])
      (Right line) => do
        (n ** lines) <- fReadToEOF file
        pure $ (S n ** line :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  ef <- openFile filename Read
  case ef of
    (Left fileError) => do
      putStrLn $ "File error: " ++ show fileError
      pure (_ ** [])
    (Right file) => do
      lines <- fReadToEOF file
      closeFile file
      pure lines
