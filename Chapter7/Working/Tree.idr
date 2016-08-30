
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

[alt] Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node l e r) = let right = foldr func init r
                                 in  foldr func (func e right) l

t : Tree Integer
t = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

-- [2,3,1]
ta : List Integer
ta = foldr (::) [] t

-- [1,2,3]
ta' : List Integer
ta' = foldr @{alt} (::) [] t
