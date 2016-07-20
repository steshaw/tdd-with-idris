data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

vtake : (taken : Nat) -> Vect (taken + m) a -> Vect taken a
vtake Z [] = []
vtake Z (x :: xs) = []
vtake (S n) (x :: xs) = x :: vtake n xs
