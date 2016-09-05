module ArithStreams

import Data.Primitives.Views

export total
randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

export total
arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound n with (divides n 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1
