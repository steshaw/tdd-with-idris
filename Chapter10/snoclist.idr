
data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (xs : List a) -> (x : a) -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs
snocList [] = Empty
snocList (x :: xs) with (snocList xs)
  snocList (x :: []) | Empty = Snoc [] x
  snocList (x :: (ys ++ [y])) | (Snoc ys y) = Snoc (x :: ys) y

describe_list_end : List Int -> String
describe_list_end input with (snocList input)
  describe_list_end [] | Empty = "Empty"
  describe_list_end (xs ++ [x]) | (Snoc xs x)
       = "Non-empty, initial portion = " ++ show xs

my_reverse : List a -> List a
my_reverse input with (snocList input)
  my_reverse [] | Empty = []
  my_reverse (xs ++ [x]) | (Snoc xs x) = x :: my_reverse xs
