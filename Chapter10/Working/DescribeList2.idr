module DescribeList2

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (init : List a) -> (x : a) -> ListLast (init ++ [x])

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty           => NonEmpty [] x
                          NonEmpty init y => NonEmpty (x :: init) y

total
describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (init ++ [x]) | (NonEmpty init x) = "Non-empty, initial portion = " ++ show init
