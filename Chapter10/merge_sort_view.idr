import Data.List.Views

merge_sort : Ord a => List a -> List a
merge_sort input with (splitRec input)
  merge_sort [] | SplitRecNil = []
  merge_sort [x] | SplitRecOne = [x]
  merge_sort (lefts ++ rights) | (SplitRecPair lrec rrec) 
             = merge (merge_sort lefts | lrec)
                     (merge_sort rights | rrec)
