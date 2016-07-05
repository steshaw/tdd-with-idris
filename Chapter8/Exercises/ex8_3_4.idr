data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs

{- 1 -}

head_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
           (contra : (x = y) -> Void) -> (x :: xs) = (y :: ys) -> Void
head_unequal contra Refl = contra Refl

tail_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
          (contra : (xs = ys) -> Void) -> (x :: xs) = (y :: ys) -> Void
tail_unequal contra Refl = contra Refl

{- 2 -}

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) = case decEq x y of
                                     No contra => No (head_unequal contra)
                                     Yes Refl => case decEq xs ys of
                                                      Yes Refl => Yes Refl
                                                      No contra => No (tail_unequal contra)
