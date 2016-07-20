import Data.Vect

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

read_vect : IO (VectUnknown String)
read_vect = do
  x <- getLine
  if x == ""
     then pure (MkVect _ [])
     else do
       MkVect _ xs <- read_vect
       pure $ MkVect _ (x :: xs)

print_vect : Show a => VectUnknown a -> IO ()
print_vect (MkVect len xs) =
  putStrLn (show xs ++ " (length " ++ show len ++ ")")

-- readOne : IO (Vect len String -> Vect (S len) String)
-- readOne = do
--   s <- getLine

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])

readVect : IO (n : Nat ** Vect n String)
readVect = do
  x <- getLine
  if x == ""
     then pure (0 ** [])
     else do
       (n ** xs) <- readVect
       pure $ (S n ** (x :: xs))

printVect : Show a => (n : Nat ** Vect n a) -> IO ()
printVect (len ** ss) =
  putStrLn $ show ss ++ " (length " ++ show len ++ ")"

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector:"
  (len1 ** vec1) <- readVect
  putStrLn "Enter second vector:"
  (len2 ** vec2) <- readVect
  case exactLength len1 vec2 of
    Nothing => putStrLn "lengths do not match"
    (Just vec2) => printLn (zip vec1 vec2)
