module EqNat

public export
data EqNat : (n1 : Nat) -> (n2 : Nat) -> Type where
  Same : (n : Nat) -> EqNat n n

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

export
checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (EqNat n1 n2)
checkEqNat Z Z = Just $ Same 0
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just $ sameS _ _ eq
