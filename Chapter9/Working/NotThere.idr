import Data.Vect

fromFalse : (d : Dec p) -> {auto eq : decAsBool d = False} -> Not p
fromFalse (Yes _) {eq = Refl} impossible
fromFalse (No contra) = contra

notThere : Not (Elem 4 [1, 2, 3])
notThere = fromFalse (isElem 4 [1,2,3])
