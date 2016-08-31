import Data.Vect

myReverse : Vect len a -> Vect len a
myReverse xs = reverse' [] xs
  where
    reverseProof_nil : Vect n a -> Vect (plus n 0) a
    reverseProof_nil xs {n} = rewrite plusZeroRightNeutral n in xs

    reverseProof_xs : Vect (S (n + k)) a -> Vect (n + (S k)) a
    reverseProof_xs xs {n} {k} = rewrite sym (plusSuccRightSucc n k) in xs

    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) = reverseProof_xs $ reverse' (x :: acc) xs
