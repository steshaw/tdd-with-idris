
data Tree elem
  = Empty
  | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node l e r) (Node l' e' r') = l == l' && e == e' && r == r'
  (==) _ _ = False

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node l e r) = let left  = foldr func init l
                                     right = foldr func left r
                                 in  func e right
