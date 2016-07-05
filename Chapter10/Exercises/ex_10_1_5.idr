mingle : List a -> List a -> List a
mingle [] [] = []
mingle [] xs = xs
mingle xs [] = xs
mingle (x :: xs) (y :: ys) = x :: y :: mingle xs ys

data Mingled : List a -> Type where
     MkMingled : (xs : List a) -> (ys : List a) -> Mingled (mingle xs ys)

{- 1 -}

mingled : (xs : List a) -> Mingled xs
mingled [] = MkMingled [] []
mingled (x :: []) = MkMingled [x] []
mingled (x :: (y :: xs)) = case mingled xs of
                                MkMingled xs ys => MkMingled (x :: xs) (y :: ys)

{- 2 -}

mergeSort : Ord a => List a -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs with (mingled xs)
  mergeSort (mingle ys zs) | (MkMingled ys zs) =
       merge (mergeSort ys) (mergeSort zs)
