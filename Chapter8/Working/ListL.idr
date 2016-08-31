module ListL

public export
data ListL : Nat -> Type -> Type where
  Nil  : ListL Z a
  (::) : a -> ListL len a -> ListL (S len) a

%name ListL xs, ys, zs
