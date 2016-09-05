module InfList

namespace LazyList

  public export
  data LazyList : Type -> Type where
    (::) : (value : elem) -> Lazy (LazyList elem) -> LazyList elem

  %name LazyList xs, ys, zs

  export
  countFrom : Integer -> LazyList Integer
  countFrom x = x :: countFrom (x + 1)

  export
  getPrefix : (count : Nat) -> LazyList ty -> List ty
  getPrefix Z xs = []
  getPrefix (S k) (value :: xs) = value :: getPrefix k xs

namespace InfList

  public export
  data InfList : Type -> Type where
    (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

  %name InfList xs, ys, zs

  export
  countFrom : Integer -> InfList Integer
  countFrom x = x :: countFrom (x + 1)

  export
  getPrefix : (count : Nat) -> InfList ty -> List ty
  getPrefix Z xs = []
  getPrefix (S k) (value :: xs) = value :: getPrefix k xs
