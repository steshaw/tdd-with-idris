data SnocList ty = Empty | Snoc (SnocList ty) ty

reverse_snoc : SnocList ty -> List ty
reverse_snoc Empty = []
reverse_snoc (Snoc xs x) = x :: reverse_snoc xs
