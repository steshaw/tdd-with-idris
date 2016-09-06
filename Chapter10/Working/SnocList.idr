module SnocList

data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : SnocList xs -> (ys : List a) -> SnocList (xs ++ ys)
snocListHelp {xs} snoc [] = rewrite appendNilRightNeutral xs in snoc
snocListHelp {xs} snoc (y :: ys) = rewrite appendAssociative xs [y] ys in
                                           snocListHelp (Snoc snoc {x}) ys

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelp : (input : List a) -> SnocList input -> List a
myReverseHelp [] Empty = []
myReverseHelp (xs ++ [x]) (Snoc rec) = x :: myReverseHelp xs rec

myReverse : List a -> List a
myReverse xs = myReverseHelp xs (snocList xs)
