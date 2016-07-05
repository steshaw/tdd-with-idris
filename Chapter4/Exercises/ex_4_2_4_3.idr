data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

{- 3 and 4-}

take : (n : Nat) -> Vect (n + m) a -> Vect n a
take Z xs = []
take (S k) (x :: xs) = x :: take k xs
