data Tree elem
  = Empty
  | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => t
                                    GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree l = foldl (flip insert) Empty l

treeToList : Tree a -> List a
treeToList Empty = Nil
treeToList (Node left v right) =
  treeToList left ++ [v] ++ treeToList right

data Expr
  = EInt Integer
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

%name Expr e1, e2, e3

evaluate : Expr -> Integer
evaluate (EInt i) = i
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing r@(Just x) = r
maxMaybe l@(Just x) Nothing = l
maxMaybe (Just x) (Just y) = Just $ max x y

data Shape
  = Triangle Double Double
  | ||| A rectangle, with its length and height
  Rectangle Double Double
  | ||| A circle, with its radius
  Circle Double
%name Shape shape1, shape2, shape3

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture
  = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture
%name Picture pic1, pic2, pic3

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle x y)) = Just $ area triangle
biggestTriangle (Primitive (Rectangle x y)) = Nothing
biggestTriangle (Primitive (Circle x)) = Nothing
biggestTriangle (Combine pic1 pic2) =
  maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
