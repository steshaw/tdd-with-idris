twoplustwo_not_five : 2 + 2 = 5 -> Void
twoplustwo_not_five Refl impossible

value_not_suc : (x : Nat) -> x = S x -> Void
value_not_suc _ Refl impossible

loop : Void
loop = loop

nohead : Void
nohead = getHead []
  where
    getHead : List Void -> Void
    getHead (x :: xs) = x

