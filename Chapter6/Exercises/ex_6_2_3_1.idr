import Data.Vect

{- 1 -}

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)
