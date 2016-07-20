import Data.Vect

{- 1 -}

my_plusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
my_plusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
my_plusCommutes (S k) m = rewrite my_plusCommutes k m in
                          rewrite plusSuccRightSucc m k in Refl

{- 2 -}

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect (S n + k) a -> Vect (plus n (S k)) a
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

my_reverse : Vect n a -> Vect n a
my_reverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs)
                        = reverseProof_xs (reverse' (x::acc) xs)
