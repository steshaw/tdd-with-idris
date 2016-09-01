import ListL

data Elem : a -> ListL k a -> Type where
  Here  : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

not_in_nil : Elem value [] -> Void
not_in_nil Here impossible
not_in_nil (There _) impossible

not_in_tail : (notHere : (value = x) -> Void) ->
              (notThere : Elem value xs -> Void) ->
              Elem value (x :: xs) ->
              Void
not_in_tail notHere notThere Here = notHere Refl
not_in_tail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : ListL n a) -> Dec (Elem value xs)
isElem value [] = No not_in_nil
isElem value (x :: xs) = case decEq value x of
                              Yes Refl   => Yes Here
                              No notHere => case isElem value xs of
                                                 Yes prf     => Yes (There prf)
                                                 No notThere => No (not_in_tail notHere notThere)
