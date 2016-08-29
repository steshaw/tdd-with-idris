data Expr num
  = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs $ eval x

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

surround : Show x => x -> Show y => y -> String -> String
surround x y op = "(" ++ show x ++ " " ++ op ++ " " ++ show y ++ ")"

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = surround x y "+"
  show (Sub x y) = surround x y "-"
  show (Mul x y) = surround x y "*"
  show (Div x y) = surround x y "/"
  show (Abs x) = "abs(" ++ show x ++ ")"

(Eq ty, Neg ty, Integral ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

(Neg num, Integral num) => Cast (Expr num) num where
  cast e = eval e
