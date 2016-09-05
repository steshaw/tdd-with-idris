
labelWith : Stream t -> List a -> List (t, a)
labelWith xs [] = []
labelWith (x :: xs) (y :: ys) = (x, y) :: labelWith xs ys

label : List a -> List (Integer, a)
label = labelWith [0..]
