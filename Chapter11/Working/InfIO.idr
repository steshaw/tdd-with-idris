module InfIO

%default total

public export
data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

public export
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

loopPrint : String -> InfIO
loopPrint msg = do
  putStrLn msg
  loopPrint msg

namespace Run

  partial
  run : InfIO -> IO ()
  run (Do action k) = do
    res <- action
    run (k res)

namespace Limited

  data Fuel = Dry | More Fuel

  tank : Nat -> Fuel
  tank Z = Dry
  tank (S k) = More (tank k)

  run : Fuel -> InfIO -> IO ()
  run Dry _                     = putStrLn "Out of fuel"
  run (More fuel) (Do action k) = do res <- action
                                     run fuel (k res)

namespace Infinite

  export
  data Fuel = Dry | More (Lazy Infinite.Fuel)

  export partial
  forever : Infinite.Fuel
  forever = More forever

  export
  run : Infinite.Fuel -> InfIO -> IO ()
  run Dry _                     = putStrLn "Out of fuel"
  run (More fuel) (Do action k) = do res <- action
                                     run fuel (k res)
