%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

loop_print : String -> InfIO
loop_print msg = do putStrLn msg
                    loop_print msg

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

partial
runPartial : InfIO -> IO ()
runPartial (Do action f) = do res <- action
                              runPartial (f res)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = putStrLn "Out of fuel"

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = run (tank 10) (loop_print "vroom")
