{- 1 -}

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : Elem x xs -> Elem x (y :: xs)

{- 2 -}

data Last : List a -> a -> Type where
     LastOne : Last [item] val
     LastCons : (prf : Last xs val) -> Last (x :: xs) val

lastNotNil : (value : a) -> Last [] x -> Void
lastNotNil _ LastOne impossible
lastNotNil _ (LastCons _) impossible

lastNotCons : (contra : Last (x :: xs) value -> Void) ->
              Last (y :: (x :: xs)) value -> Void
lastNotCons contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No (lastNotNil value)
isLast (y :: []) value = case decEq y value of
                              (Yes prf) => Yes LastOne
                              (No contra) => ?isLast_rhs_4
isLast (y :: (x :: xs)) value = case isLast (x :: xs) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (lastNotCons contra)
