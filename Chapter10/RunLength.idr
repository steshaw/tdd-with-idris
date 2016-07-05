
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
