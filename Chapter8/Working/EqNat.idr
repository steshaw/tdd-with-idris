module EqNat

export
checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (n1 = n2)
checkEqNat Z Z = Just $ Refl {x = 0}
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just $ cong eq
