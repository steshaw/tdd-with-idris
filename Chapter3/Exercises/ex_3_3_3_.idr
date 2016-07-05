module ex_3_3_3_

import Data.Vect

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

{-
|1 2|   | 7  8  9 10|  = | 29  32  35  38|
|3 4| x |11 12 13 14|    | 65  72  79  86|
|5 6|                    |101 112 123 134|

1 2     7 11  1 x 7 + 2 x 11
3 4  x  8 12
5 6     9 13
       10 14
-}
smush : Num a => (x : Vect m a) -> (y : Vect m a) -> a
smush x y = sum $ zipWith (*) x y

row : Num a => (x : Vect m a) -> (ys : Vect p (Vect m a)) -> Vect p a
row x ys = do
  y <- ys
  pure $ (smush x y)

multMatrixTrans : Num a => (xs : Vect n (Vect m a)) ->
                  (yts : Vect p (Vect m a)) ->
                  Vect n (Vect p a)
multMatrixTrans [] _ = []
multMatrixTrans (x :: xs) ys = row x ys :: multMatrixTrans xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) ->
             Vect n (Vect p a)
multMatrix xs ys = let yts = transpose_mat ys in
                  multMatrixTrans xs yts
