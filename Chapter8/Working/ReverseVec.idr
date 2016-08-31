import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) = rewrite plusCommutative 1 k in reverse xs ++ [x]

myReverse' : Vect n elem -> Vect n elem
myReverse' [] = []
myReverse' (x :: xs) = reverseProof $ myReverse' xs ++ [x]
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} ys = rewrite plusCommutative 1 k in ys
