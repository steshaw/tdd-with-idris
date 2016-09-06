module SnocList

data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

total
snocListHelp : SnocList xs -> (ys : List a) -> SnocList (xs ++ ys)
snocListHelp {xs} snoc [] = rewrite appendNilRightNeutral xs in snoc
snocListHelp {xs} snoc (y :: ys) = rewrite appendAssociative xs [y] ys in
                                           snocListHelp (Snoc snoc {x}) ys

total
snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

total
myReverseHelp : (input : List a) -> SnocList input -> List a
myReverseHelp [] Empty = []
myReverseHelp (xs ++ [x]) (Snoc rec) = x :: myReverseHelp xs rec

total
myReverse : List a -> List a
myReverse xs = myReverseHelp xs (snocList xs)

total
myReverse' : List a -> List a
myReverse' xs with (snocList xs)
  myReverse' [] | Empty = []
  myReverse' (ys ++ [x]) | (Snoc rec) = x :: myReverse' ys | rec
