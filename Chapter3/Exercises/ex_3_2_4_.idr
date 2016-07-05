module ex_3_2_4_

import Data.Vect

total
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

total
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = reverse xs ++ [x]

total
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

namespace Vect
  total
  my_map : (a -> b) -> Vect n a -> Vect n b
  my_map f [] = []
  my_map f (x :: xs) = f x :: my_map f xs
