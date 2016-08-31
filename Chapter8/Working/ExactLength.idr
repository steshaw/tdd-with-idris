import EqNat
import ListL

data ListL : Nat -> Type -> Type where
  Nil  : ListL Z a
  (::) : a -> ListL len a -> ListL (S len) a

exactLength : (len : Nat) -> (input : ListL l a) -> Maybe (ListL len a)
exactLength len input {l} = case checkEqNat l len of
                                 Nothing => Nothing
                                 Just Refl => Just input

exactLength' : (len : Nat) -> (input : ListL l a) -> Maybe (ListL len a)
exactLength' {l} len input = case decEq l len of
                                  Yes Refl => Just input
                                  No contra => Nothing
