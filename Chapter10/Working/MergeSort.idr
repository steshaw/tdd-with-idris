module MergeSort

data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitList : (xs : List a) -> SplitList xs
splitList xs = splitListHelp xs xs
  where
    total
    splitListHelp : (stepper : List a) -> (xs : List a) -> SplitList xs
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: stepper) (x :: xs) = 
      case splitListHelp stepper xs of
           SplitNil => SplitOne
           SplitOne {x = x2} => SplitPair [x] [x2]
           SplitPair lefts rights => SplitPair (x :: lefts) rights
    splitListHelp _ xs = SplitPair [] xs

mergeSort : Ord a => List a -> List a
mergeSort xs with (splitList xs)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)
