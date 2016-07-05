data SplitList : List a -> Type where
     MkSplit : (lefts : List a) -> (rights : List a) ->
               SplitList (lefts ++ rights)

total
splitList : (input : List a) ->  SplitList input
splitList input = splitListHelp input input
  where
    splitListHelp : (counter : List a) -> (input : List a) -> SplitList input
    splitListHelp (_ :: _ :: ys) [] = MkSplit [] []
    splitListHelp (_ :: _ :: ys) (x :: xs)
       = case splitListHelp ys xs of
              MkSplit lefts rights => MkSplit (x :: lefts) rights
    splitListHelp _ xs = MkSplit [] xs


mergeSort : Ord a => List a -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort input with (splitList input)
  mergeSort (lefts ++ rights) | MkSplit lefts rights =
     let sortedLeft = mergeSort lefts
         sortedRight = mergeSort rights in
         merge sortedLeft sortedRight
