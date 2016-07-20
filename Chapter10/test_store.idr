import DataStore

test_store : DataStore (SString .+. SString .+. SInt)
test_store = addToStore ("Mercury", "Mariner 10", 1974) $
             addToStore ("Venus", "Venera", 1961) $
             addToStore ("Uranus", "Voyager 2", 1986) $
             addToStore ("Pluto", "New Horizons", 2015) $
             empty

showItems : DataStore schema -> List (SchemaType schema)
showItems input with (storeView input)
  showItems empty | SNil = []
  showItems (addToStore entry store) | (SAdd rec) 
         = entry :: showItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
  filterKeys test input | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) 
       = if test value 
            then key :: filterKeys test store | rec
            else filterKeys test store | rec
