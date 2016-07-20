import Data.Vect


my_reverse1 : Vect n a -> Vect n a
my_reverse1 [] = []
my_reverse1 {n = S k} (x :: xs)
        = let result = my_reverse1 xs ++ [x] in
              rewrite plusCommutative 1 k in result

my_reverse : Vect n a -> Vect n a
my_reverse [] = []
my_reverse (x :: xs) = reverseProof (my_reverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) a -> Vect (S k) a
    reverseProof {k} result = rewrite plusCommutative 1 k in result
