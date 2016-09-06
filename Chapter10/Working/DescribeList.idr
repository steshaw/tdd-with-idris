module DescribeList

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (init : List a) -> (x : a) -> ListLast (init ++ [x])

total
describeHelp : (input : List Int) -> (form : ListLast input) -> String
describeHelp [] Empty = "Empty"
describeHelp (init ++ [x]) (NonEmpty init x) = "Non-empty, initial portion = " ++ show init

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty           => NonEmpty [] x
                          NonEmpty init y => NonEmpty (x :: init) y

total
describeListEnd : List Int -> String
describeListEnd xs = describeHelp xs (listLast xs)
