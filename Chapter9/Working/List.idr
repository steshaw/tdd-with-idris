data Elem : a -> List a -> Type where
  Head : Elem x (x :: xs)
  Tail : Elem x xs -> Elem x (_ :: xs)

atHead : Elem 1 [1..3]
atHead = Head

atMiddle : Elem 2 [1..3]
atMiddle = Tail Head

notInEmpty : Elem 4 [] -> Void
notInEmpty Head impossible
notInEmpty (Tail _) impossible

notThere : Elem 4 [1..3] -> Void
notThere (Tail (Tail (Tail Head))) impossible
notThere (Tail (Tail (Tail (Tail _)))) impossible

atLast : Elem "Mary" ["Peter", "Paul", "Mary"]
atLast = Tail (Tail Head)

data Last : List a -> a -> Type where
  LastOne  : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1, 2, 3] 3
last123 = LastCons (LastCons LastOne)

lastOnEmpty : Last [] value -> Void
lastOnEmpty LastOne impossible
lastOnEmpty (LastCons _) impossible

rhs : (contra2 : Last xs value -> Void) -> (contra1 : (xs = [value]) -> Void) -> Dec (Last (x :: xs) value)
rhs contra2 contra1 = ?rhs_rhs_2

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No lastOnEmpty
isLast (x :: xs) value = case decEq xs [value] of
                              Yes Refl => Yes (LastCons LastOne)
                              No contra1 => case isLast xs value of
                                                Yes prf => Yes (LastCons prf)
                                                No contra2 => rhs contra2 contra1
