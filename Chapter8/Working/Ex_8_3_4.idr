import ListL

total
headUnequal : DecEq a => {xs : ListL n a} -> {ys : ListL n a} ->
                         (contra : x = y -> Void) ->
                         (x :: xs = y :: ys) -> Void
headUnequal contra Refl = contra Refl

total
tailUnequal : DecEq a => {xs : ListL n a} -> {ys : ListL n a} ->
                         (contra : xs = ys -> Void) ->
                         (x :: xs = y :: ys -> Void)
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (ListL n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
         Yes Refl  => case decEq xs ys of
                           Yes Refl  => Yes Refl
                           No contra => No (tailUnequal contra)
         No contra => No $ headUnequal contra
