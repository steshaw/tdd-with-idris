module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size _) = size

item : (store : DataStore) -> Vect (size store) String
item (MkData _ items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: (addToData xs)

data Command
  = Add String
  | Get Integer
  | Search String
  | Size
  | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just $ Add str
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just $ Search str
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
  (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry id store@(MkData size items) =
case integerToFin id size of
  Nothing => Just ("Out of range\n", store)
  (Just x) => Just (index x items ++ "\n", store)

stringIntercalate : String -> List String -> String
stringIntercalate sep ss = pack $ intercalate (unpack sep) (map unpack ss)

toIndexedPairs : (idx : Nat) -> (items : Vect size String) -> List (Nat, String)
toIndexedPairs idx [] = []
toIndexedPairs idx (x :: xs) = (idx, x) :: toIndexedPairs (S idx) xs

searchCommand : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchCommand str store@(MkData size items) = Just (result, store)
  where
    results : List (Nat, String)
    results = filter (isInfixOf str . snd) (toIndexedPairs 0 items)
    results' : List String
    results' = map showIt results
      where
        showIt (idx, item) = (show idx) ++ " : " ++ item
    result : String
    result = stringIntercalate ", " results' ++ "\n"

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID" ++ show (size store) ++ "\n",
                             addToStore store item)
    Just (Get id) => getEntry id store
    Just (Search str) => searchCommand str store
    Just Size => Just ("Size = " ++ show (size store) ++ "\n", store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
