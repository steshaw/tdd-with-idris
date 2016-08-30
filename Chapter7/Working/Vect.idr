data ListL : (length : Nat) -> Type -> Type where
  Nil   : ListL 0 a
  (::)  : a -> ListL n a -> ListL (S n) a

Eq a => Eq (ListL n a) where
  [] == [] = True
  (x :: xs) == (y :: ys) = x == y && xs == ys

Foldable (ListL n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
