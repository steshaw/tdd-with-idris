module matrix_

import Data.Vect

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_helper : (x : Vect n elem) ->
                   (xs_trans : Vect n (Vect k elem)) ->
                   Vect n (Vect (S k) elem)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          transpose_helper x xs_trans
