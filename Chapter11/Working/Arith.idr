import ArithStreams

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (n1 :: n2 :: nums) score = do
  putStrLn $ "Score so far: " ++ show score
  putStr $ show n1 ++ " * " ++ show n2 ++ "? "
  answer <- getLine
  if cast answer == n1 * n2
     then do putStrLn "Correct!"
             quiz nums (score + 1)
     else do putStrLn $ "Wrong, the answer is " ++ show (n1 * n2)
             quiz nums score
