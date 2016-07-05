data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : {xs : List a} -> {x : a} -> SnocList (xs ++ [x])

snocList : (xs : List a) -> SnocList xs
snocList [] = Empty
snocList (x :: xs) with (snocList xs)
  snocList (x :: []) | Empty = Snoc {xs = []} {x = x}
  snocList (x :: (ys ++ [y])) | Snoc = Snoc {xs = x :: ys} {x = y}

{- 1 -}

equalSuffix : DecEq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | Snoc with (snocList ys)
    equalSuffix (zs ++ [x]) [] | Snoc | Empty = []
    equalSuffix (zs ++ [x]) (xs ++ [y]) | Snoc | Snoc with (decEq x y)
      equalSuffix (zs ++ [x]) (xs ++ [x]) | Snoc | Snoc | (Yes Refl) = equalSuffix zs xs ++ [x]
      equalSuffix (zs ++ [x]) (xs ++ [y]) | Snoc | Snoc | (No contra) = []

{- 2 -}

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact {n_xs = []}
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | Exact = Exact {n_xs = x :: n_xs}

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | Exact = n_xs :: groupByN n rest

{- 3 -}

halve : List a -> (List a, List a)
halve xs with (takeN (length xs `div` 2) xs)
  halve xs | Fewer = (xs, [])
  halve (n_xs ++ rest) | Exact = (n_xs, rest)
