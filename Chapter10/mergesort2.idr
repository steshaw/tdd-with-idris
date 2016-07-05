
data SplitList : List a -> Type where
     MkSplit : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

splitListHelp : (counter : List a) -> (xs : List a) -> SplitList xs
splitListHelp (_ :: _ :: ys) [] = MkSplit [] []
splitListHelp (_ :: _ :: ys) (x :: xs)
   = case splitListHelp ys xs of
          (MkSplit lefts rights) => MkSplit (x :: lefts) rights
splitListHelp _ xs = MkSplit [] xs

splitList : (xs : List a) ->  SplitList xs
splitList xs = splitListHelp xs xs

mutual
  doMerge : Ord a => (xs : List a) -> SplitList xs -> List a
  doMerge (lefts ++ rights) (MkSplit _ _)
     = let sortedLeft = mergeSort lefts
           sortedRight = mergeSort rights in
           merge sortedLeft sortedRight

  mergeSort : Ord a => List a -> List a
  mergeSort [] = []
  mergeSort [x] = [x]
  mergeSort input = doMerge input (splitList input)
