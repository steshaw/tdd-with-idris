import Data.Vect

oneInVector : Elem 1 [1,2,3]
oneInVector = Here

notThere : Elem 4 [1,2,3] -> Void
notThere (There (There (There Here))) impossible
notThere (There (There (There (There _)))) impossible

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There $ There Here

removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf: Elem value xs) ->
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem value (y :: []) (There later) {n = Z} = absurd later
removeElem value (y :: ys) (There later) {n = (S k)} = y :: removeElem value ys later

removeElem_auto : (value : a) ->
                  (xs : Vect (S n) a) ->
                  {auto prf: Elem value xs} ->
                  Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf
