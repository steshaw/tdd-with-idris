data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty xs y => NonEmpty (x :: xs) y

my_reverse : List a -> List a
my_reverse input with (listLast input)
  my_reverse [] | Empty = []
  my_reverse (xs ++ [x]) | (NonEmpty xs x) = x :: my_reverse xs


-- my_reverse xs with (listLast xs)
--   my_reverse [] | Empty = []
--   my_reverse (ys ++ [x]) | (NonEmpty ys x) = x :: my_reverse ys
