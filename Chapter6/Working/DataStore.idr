module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: (addToData xs)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _  => case parseSchema xs of
                                               Nothing => Nothing
                                               Just schemaR => Just (SString .+. schemaR)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _  => case parseSchema xs of
                                            Nothing => Nothing
                                            Just schemaR => Just (SInt .+. schemaR)
parseSchema _ = Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                              (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                              _                     => Nothing
    getQuoted _           = Nothing
parsePrefix SInt input = case span isDigit input of
                           ("", rest)  => Nothing
                           (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemaL .+. schemaR) input =
  case parsePrefix schemaL input of
    Nothing => Nothing
    Just (lVal, rest) =>
      case parsePrefix schemaR rest of
        Nothing => Nothing
        Just (rVal, rest') => Just ((lVal, rVal), rest')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = 
  case parsePrefix schema input of
    Just (res, "") => Just res
    Just _         => Nothing
    Nothing        => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "schema" str = case parseSchema (words str) of
                                        Nothing => Nothing
                                        Just schemaOk => Just (SetSchema schemaOk)
parseCommand schema "add" str = case parseBySchema schema str of
                                  Nothing => Nothing
                                  Just item => Just (Add item)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                  False => Nothing
                                  True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
  (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) 
        = "(" ++ display iteml ++ ", " ++ display itemr ++ ")"

getEntry : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry id store@(MkData schema size items) =
  case integerToFin id size of
    Nothing => Just ("Out of range\n", store)
    (Just id) => Just $ (display (index id items) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z   => Just (MkData schema _ [])
                              S k => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (SetSchema newSchema) => case setSchema store newSchema of
                                     Nothing => Just ("Can't update schema\n", store)
                                     Just newStore => Just ("OK\n", newStore)
    Just (Add item) => Just ("ID" ++ show (size store) ++ "\n",
                             addToStore store item)
    Just (Get id) => getEntry id store
    Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
