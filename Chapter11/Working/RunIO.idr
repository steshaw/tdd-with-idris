module RunIO

%default total

public export
data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do   : IO a -> (a -> Inf (RunIO b)) -> RunIO b

public export
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

public export
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

export
run : Fuel -> RunIO a -> IO (Maybe a)
run _ (Quit x) = pure $ Just x
run Dry _ = pure Nothing
run (More fuel) (Do action k) = do res <- action
                                   run fuel (k res)

greet : RunIO ()
greet = do
  putStr "Enter your name: "
  name <- getLine
  if name == ""
     then do putStrLn "Bye!"
             Quit ()
     else do putStrLn ("Hello " ++ name)
             greet

partial
main : IO ()
main = do
  run forever greet
  pure ()
