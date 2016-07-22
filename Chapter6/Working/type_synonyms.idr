import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(?tri_rhs1, ?tri_rhs2), (?tri_rhs3, ?tri_rhs4), (?tri_rhs5, ?tri_rhs6)]
