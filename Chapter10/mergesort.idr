splitList : (input : List a) ->  (List a, List a)
splitList input = splitListHelp input input
  where
    splitListHelp : (counter : List a) ->
                    (input : List a) -> (List a, List a)
    splitListHelp (_ :: _ :: counter) [] = ([], [])
    splitListHelp (_ :: _ :: counter) (item :: items)
       = case splitListHelp counter items of
              (lefts, rights) => (item :: lefts, rights)
    splitListHelp _ items = ([], items)

mergeSort : Ord a => (xs : List a) -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort input = case splitList input of
                      (lefts, rights) =>
                          let sortedLeft = mergeSort lefts
                              sortedRight = mergeSort rights in
                              merge sortedLeft sortedRight 
