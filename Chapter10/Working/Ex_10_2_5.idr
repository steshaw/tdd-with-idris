import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

-- 1.

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys = reverse $ equalSuffix' xs ys
  where
    total
    equalSuffix' : Eq a => List a -> List a -> List a
    equalSuffix' xs ys with (snocList xs)
      equalSuffix' [] ys | Empty = []
      equalSuffix' (xs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
        equalSuffix' (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
        equalSuffix' (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) =
          if x == y then x :: equalSuffix' xs ys | xsrec | ysrec
                    else []
                    
-- 2.

total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ls ++ rs) | (SplitRecPair lrec rrec) =
    merge (mergeSort ls | lrec) (mergeSort rs | rrec)

-- 3.

total
toBinary : Nat -> String
toBinary Z = "0"
toBinary n@(S k) = toBinary' n
  where
    toBinary' : Nat -> String
    toBinary' n with (halfRec n)
      toBinary' Z | HalfRecZ = ""
      toBinary' (x + x) | (HalfRecEven rec) = toBinary' x | rec ++ "0"
      toBinary' (S (x + x)) | (HalfRecOdd rec) = toBinary' x | rec ++ "1"
