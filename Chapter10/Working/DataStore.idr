module DataStore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString
            | SChar
            | SInt
            | (.+.) Schema Schema

public export
total
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

export
record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)

export
empty : DataStore schema
empty = MkData 0 []

export
total
addToStore : (item : SchemaType schema) -> (store : DataStore schema) -> DataStore schema
addToStore item (MkData _ items) = MkData _ (item :: items)

public export
data StoreView : DataStore schema -> Type where
  SNil : StoreView empty
  SAdd : (rec : StoreView store) -> StoreView (addToStore item store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (x :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelp items
