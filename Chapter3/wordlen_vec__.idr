module wordlen_vec__

import Data.Vect

total
word_lengths : Vect len String -> Vect len Nat
word_lengths [] = []
word_lengths (w :: ws) = length w :: word_lengths ws
