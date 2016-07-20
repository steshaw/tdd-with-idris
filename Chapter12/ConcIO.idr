import System.Concurrency.Channels

data Server : (iface : request -> Type) -> Type where
     MkServer : (process : PID) -> Server iface

mutual
  -- Sequences of IO commands which can spawn concurrent processes
  data ConcIO : (iface : request -> Type) -> Type -> Type where
       -- Send a message on a channel and wait for a reply (if it's a server
       -- channel)
       Send : {server_iface : serverreq -> Type} ->
              Server server_iface -> (msg : serverreq) ->
              ConcIO iface (Maybe (server_iface msg))

       -- Process a client message (must terminate!)
       -- If there's no message, does nothing
       Accept : ((r : request) -> ConcIO iface (iface r)) -> ConcIO iface ()

       -- Spawn a process (which can't terminate since it's a Process)
       -- and return a channel we can send messages on
       Fork : Process server_iface () -> ConcIO iface (Server server_iface)

       -- Some plumbing, to allow arbitrary IO actions and to bind sequences
       -- of commands together
       Action : IO a -> ConcIO iface a
       Pure : a -> ConcIO iface a
       (>>=) : ConcIO iface a -> (a -> ConcIO iface b) -> ConcIO iface b

  -- A concurrent process is an infinite sequence of concurrent IO
  -- commands
  data Process : (iface : request -> Type) -> Type -> Type where
       Do : ConcIO iface a -> (a -> Inf (Process iface b)) -> Process iface b

namespace ProcessDo
  (>>=) : ConcIO iface a -> (a -> Inf (Process iface b)) -> Process iface b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

mutual
  total
  runConc : Fuel -> ConcIO iface t -> IO t 
  runConc fuel (Send {server_iface} (MkServer process) msg) 
          = do Just chan <- connect process
                    | _ => pure Nothing
               ok <- unsafeSend chan msg
               if ok then unsafeRecv (server_iface msg) chan
                     else pure Nothing
  runConc fuel (Accept {request} f) 
          = do Just sender <- listen 1
                    | Nothing => pure () -- nothing connecting
               Just msg <- unsafeRecv request sender
                    | Nothing => pure () -- no message received
               res <- runConc fuel (f msg)
               unsafeSend sender res
               pure ()
  runConc fuel (Fork x) = do pid <- spawn (do run fuel x; pure ())
                             pure (MkServer pid)
  runConc fuel (Action x) = x
  runConc fuel (Pure x) = pure x
  runConc fuel (x >>= f) = do x' <- runConc fuel x
                              runConc fuel (f x')

  total
  run : Fuel -> Process iface a -> IO (Maybe a)
  run (More fuel) (Do c f) = do res <- runConc fuel c
                                run fuel (f res)
  run Dry p = pure Nothing


------ An example

data Stuff = Negate Int | Add Int Int | Append (List a) (List a)

StuffResp : Stuff -> Type
StuffResp (Negate x) = Int
StuffResp (Add x y) = Int
StuffResp (Append {a} xs ys) = List a

stuffServer : Process StuffResp ()
stuffServer = do Action (putStrLn "Awaiting ConcIO")
                 Accept (\cmd => 
                           case cmd of
                             Negate x => do Action $ putStrLn "Negate"
                                            Pure (-x)
                             Add x y => do Action $ putStrLn "Add"
                                           Pure (x + y)
                             Append xs ys => do Action $ putStrLn "Append"
                                                Pure (xs ++ ys))
                 -- Action (putStrLn "Processed input, awaiting another")
                 stuffServer

stuffClient : Server StuffResp -> Process (const Void) ()
stuffClient s = do Action (putStr ": ")
                   x <- Action getLine
                   Just res <- Send s (Add 42 (cast x))
                       | Nothing => -- restart server
                           do Action (putStrLn "No answer")
                              server' <- Fork stuffServer
                              stuffClient server'
                   Action (putStrLn (show res))
                   stuffClient s

stuffMain : Process (const {b=Void} Void) ()
stuffMain = do s <- Fork stuffServer
               stuffClient s

main : IO ()
main = do run forever stuffMain
          pure ()
