
data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : {xs : List a} -> {x : a} -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs
snocList [] = Empty
snocList (x :: xs) with (snocList xs)
  snocList (x :: []) | Empty = Snoc {xs = []} {x = x}
  snocList (x :: (ys ++ [y])) | Snoc = Snoc {xs = x :: ys} {x = y}

my_reverse : List a -> List a
my_reverse input with (snocList input)
  my_reverse [] | Empty = ?my_reverse_rhs_1
  my_reverse (xs ++ [x]) | Snoc = ?my_reverse_rhs_2

isPalindrome : DecEq a => List a -> Bool
isPalindrome [] = True
isPalindrome (x :: xs) with (snocList xs)
  isPalindrome (x :: []) | Empty = True
  isPalindrome (x :: (ys ++ [y])) | Snoc with (decEq x y)
    isPalindrome (x :: (ys ++ [x])) | Snoc | (Yes Refl) = isPalindrome ys
    isPalindrome (x :: (ys ++ [y])) | Snoc | (No contra) = False
