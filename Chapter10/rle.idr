module Main

import System


data RunLength : List a -> Type where
     Empty : RunLength []
     Repeat : RunLength (replicate n val ++ xs)

data Replicated : a -> List a -> Type where
     RepeatZ : Replicated val (replicate Z val ++ xs)
     RepeatS : Replicated val (replicate (S n) val ++ xs)

replicated : DecEq a => (val : a) -> (xs : List a) -> Replicated val xs
replicated val [] = RepeatZ
replicated val (x :: xs) with (decEq val x)
  replicated val (val :: xs) | (Yes Refl) with (replicated val xs)
    replicated val (val :: xs) | (Yes Refl) | RepeatZ = RepeatS {n=0}
    replicated val (val :: (replicate (S n) val) ++ xs) | (Yes Refl) | RepeatS
            = RepeatS {n=S n}
  replicated val (x :: xs) | (No contra) = RepeatZ

runLength : DecEq a => (xs : List a) -> RunLength xs
runLength [] = Empty
runLength (x :: xs) with (replicated x xs)
  runLength (x :: xs) | RepeatZ = Repeat {n=1} {val=x}
  runLength (x :: (replicate (S n) x) ++ ys) | RepeatS
          = Repeat {n = 2 + n} {val = x}

describeRuns : DecEq a => List a -> List (Nat, a)
describeRuns xs with (runLength xs)
  describeRuns [] | Empty = []
  describeRuns ((replicate n val) ++ ys) | Repeat = (n, val) :: describeRuns ys

data Compressed : List a -> Type where
     CEmpty : Compressed []
     CRepeat : (n : Nat) -> (val : a) ->
               Compressed xs -> Compressed (replicate n val ++ xs)

mkCompress : DecEq a => (xs : List a) -> Compressed xs
mkCompress xs with (runLength xs)
  mkCompress [] | Empty = CEmpty
  mkCompress ((replicate n val) ++ ys) | Repeat = CRepeat n val (mkCompress ys)

compressToString : {xs : List Char} -> Compressed xs -> List Char
compressToString CEmpty = []
compressToString (CRepeat n val rest)
        = chr (cast n) :: val :: compressToString rest

decompressString : List Char -> (xs : List Char ** Compressed xs)
decompressString [] = (_ ** CEmpty)
decompressString (x :: []) = (_ ** CEmpty)
decompressString (num :: (val :: xs))
        = let (_ ** xs') = decompressString xs in
              (_ ** CRepeat (cast (ord num)) val xs')

compress : String -> String
compress input = pack $ compressToString $ mkCompress (unpack input)

decompress : String -> String
decompress input = pack $ fst $ decompressString (unpack input)

processFile : (String -> String) -> (input : String) -> (output : String) -> IO ()
processFile fn infile outfile
     = do Right content <- readFile infile
                               | putStrLn "File read error"
          Right () <- writeFile outfile (fn content)
                               | putStrLn "File write error"
          pure ()

main : IO ()
main = do args <- getArgs
          case args of
               (exec :: "--compress" :: file :: outfile :: []) =>
                     processFile compress file outfile
               (exec :: "--decompress" :: file :: outfile :: []) =>
                     processFile decompress file outfile
               _ => putStrLn "Invalid arguments"
