import Arith
import InfList

-- 1.

total
everyOther : Stream a -> Stream a
everyOther (x1 :: x2 :: xs) = x2 :: everyOther xs

-- 2.

Functor InfList where
  map f (x :: xs) = f x :: map f xs

-- 3.

data Face = Heads | Tails

getFace : Int -> Face
getFace n = if (n `mod` 2) == 0
               then Heads
               else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count randoms = take count $ map getFace randoms

-- 4.

squareRootApprox : (number : Double) -> (approx : Double) -> Stream Double
squareRootApprox number approx = iterate next approx
  where
    next approx = (approx + (number / approx)) / 2

-- 5.

squareRootBound : (max : Nat) -> (number : Double) -> 
                  (bound : Double) -> (approxs : Stream Double) ->
                  Double
squareRootBound Z number bound (approx :: approxs) = approx
squareRootBound (S k) number bound (approx :: approxs) =
  if abs (approx * approx - number) < bound
     then approx
     else squareRootBound k number bound approxs

squareRoot : (number : Double) -> Double
squareRoot number = squareRootBound 100 number 0.00000000001 (squareRootApprox number number)
