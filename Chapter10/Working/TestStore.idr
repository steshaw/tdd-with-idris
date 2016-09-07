module TestStore

import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore =
  addToStore ("Mercury", "Mariner 10", 1974) $
  addToStore ("Venus", "Venera", 1961) $
  addToStore ("Uranus", "Voyager 2", 1986) $
  addToStore ("Pluto", "New Horizons", 2015) $
  empty

showItems : DataStore schema -> List (SchemaType schema)
showItems store with (storeView store)
  showItems store | SNil = []
  showItems (addToStore item store) | (SAdd rec) = item :: showItems store | rec

filterKeys : (test : SchemaType valSchema -> Bool) -> DataStore (SString .+. valSchema) -> List String
filterKeys test store with (storeView store)
  filterKeys test store | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) = 
    if test value
       then key :: filterKeys test store | rec
       else filterKeys test store | rec
