safe_divide : Double -> Double -> Maybe Double
safe_divide x y = if y == 0 then Nothing
                            else Just (x / y)
