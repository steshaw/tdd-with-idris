
total
twoPlusTwo_not_five : 2 + 2 = 5 -> Void
twoPlusTwo_not_five Refl impossible

total
val_not_suc : (x : Nat) -> x = S x -> Void
val_not_suc _ Refl impossible
