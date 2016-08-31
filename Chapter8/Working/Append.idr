import Data.Vect

append_nil : (ys : Vect m elem) -> Vect (plus m 0) elem
append_nil {m} ys = rewrite plusCommutative m 0 in ys

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)
