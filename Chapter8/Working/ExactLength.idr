import EqNat

data ListL : Nat -> Type -> Type where
  Nil  : ListL 0 a
  (::) : a -> ListL len a -> ListL (S len) a

exactLength : (len : Nat) -> (input : ListL l a) -> Maybe (ListL len a)
exactLength len input {l} = case checkEqNat l len of
                                 Nothing => Nothing
                                 Just (Same len) => Just input
