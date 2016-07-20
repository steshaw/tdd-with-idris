import Debug.Trace

data SplitList : List a -> Type where
     SplitNil : SplitList []
     SplitOne : SplitList [x]
     SplitPair : (lefts : List a) -> (rights : List a) ->
                 SplitList (lefts ++ rights)

total
splitList : (xs : List a) ->  SplitList xs
splitList xs = splitListHelp xs xs
  where
    splitListHelp : (counter : List a) -> (xs : List a) -> SplitList xs
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: ys) (x :: xs)
       = case splitListHelp ys xs of
              SplitNil => SplitOne
              SplitOne {x=y} => SplitPair [x] [y]
              SplitPair lefts rights => SplitPair (x :: lefts) rights
    splitListHelp _ xs = SplitPair [] xs

merge_sort : Ord a => List a -> List a
merge_sort input with (splitList input)
  merge_sort [] | SplitNil = []
  merge_sort [x] | SplitOne = [x]
  merge_sort (lefts ++ rights) | (SplitPair lefts rights) 
         = merge (merge_sort lefts) (merge_sort rights)


