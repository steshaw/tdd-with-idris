data Format = IntF Format
            | DoubleF Format
            | CharF Format
            | StringF Format
            | Literal String Format
            | End

PrintfType : Format -> Type
PrintfType (IntF fmt) = (i : Int) -> PrintfType fmt
PrintfType (DoubleF fmt) = (d : Double) -> PrintfType fmt
PrintfType (CharF fmt) = (c : Char) -> PrintfType fmt
PrintfType (StringF fmt) = (s : String) -> PrintfType fmt
PrintfType (Literal str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (IntF fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (DoubleF fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (CharF fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (StringF fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Literal str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = IntF (toFormat chars)
toFormat ('%' :: 'f' :: chars) = DoubleF (toFormat chars)
toFormat ('%' :: 'c' :: chars) = CharF (toFormat chars)
toFormat ('%' :: 's' :: chars) = StringF (toFormat chars)
toFormat ('%' :: chars) = Literal "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                          Literal lit fmt => Literal (strCons c lit) fmt
                          fmt => Literal (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
