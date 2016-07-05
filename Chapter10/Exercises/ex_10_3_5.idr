{- Refined version of RunLength for question 3 -}

data RunLength : List a -> Type where
     Empty : RunLength []
     Repeat : RunLength (replicate (S n) val ++ xs)

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
  runLength (x :: xs) | RepeatZ = Repeat {n=Z} {val=x}
  runLength (x :: (replicate (S n) x) ++ ys) | RepeatS
          = Repeat {n = 1 + n} {val = x}

{- 1 -}

group : DecEq a => List a -> List (List a)
group xs with (runLength xs)
  group [] | Empty = []
  group (rep@(replicate (S n) val) ++ ys) | Repeat = rep :: group ys

{- 2 -}

removeRepeats : DecEq a => List a -> List a
removeRepeats xs with (runLength xs)
  removeRepeats [] | Empty = []
  removeRepeats ((replicate (S n) val) ++ ys) | Repeat = val :: removeRepeats ys
