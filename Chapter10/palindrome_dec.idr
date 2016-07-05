
data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : {xs : List a} -> {x : a} -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs
snocList [] = Empty
snocList (x :: xs) with (snocList xs)
  snocList (x :: []) | Empty = Snoc {xs = []} {x = x}
  snocList (x :: (ys ++ [y])) | Snoc = Snoc {xs = x :: ys} {x = y}

data Palindrome : List a -> Type where
     PalindromeEmpty : Palindrome []
     PalindromeSingle : Palindrome [x]
     PalindromeRec : Palindrome xs -> Palindrome (x :: xs ++ [x])

isPalindrome : DecEq a => (xs : List a) -> Maybe (Palindrome xs)
isPalindrome [] = Just PalindromeEmpty
isPalindrome (x :: xs) with (snocList xs)
  isPalindrome (x :: []) | Empty = Just PalindromeSingle
  isPalindrome (x :: (ys ++ [y])) | Snoc with (decEq x y)
    isPalindrome (x :: (ys ++ [x])) | Snoc | (Yes Refl)
          = do p <- isPalindrome ys
               Just ?recProof
    isPalindrome (x :: (ys ++ [y])) | Snoc | (No contra) = Nothing
