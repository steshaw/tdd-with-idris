data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty xs y => NonEmpty (x :: xs) y

describe_help : (input : List Int) -> ListLast input -> String
describe_help [] Empty = "Empty"
describe_help (xs ++ [x]) (NonEmpty xs x)
        = "Non-empty, initial portion = " ++ show xs

describe_list_end : List Int -> String
describe_list_end xs = describe_help xs (listLast xs)
