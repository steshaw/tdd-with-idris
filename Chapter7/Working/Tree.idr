
data Tree elem
  = Empty
  | Node (Tree elem) elem (Tree elem)

implementation Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node l e r) (Node l' e' r') = l == l' && e == e' && r == r'
  (==) _ _ = False
