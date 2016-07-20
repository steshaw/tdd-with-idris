import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs =
  case integerToFin i n of
    Nothing => Nothing
    (Just n) => Just $ index n xs
