module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SChar
            | SInt
            | (.+.) Schema Schema

total
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

total
addToStore : (store : DataStore) -> (SchemaType (schema store)) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: (addToData xs)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Integer -> Command schema
  GetAll    : Command schema
  Quit      : Command schema

total
parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _  => do schemaR <- parseSchema xs
                                             pure $ SString .+. schemaR
parseSchema ("Char" :: xs) = case xs of
                                    [] => Just SChar
                                    _  => do schemaR <- parseSchema xs
                                             pure $ SChar .+. schemaR
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _  => do schemaR <- parseSchema xs
                                          pure $ SInt .+. schemaR
parseSchema _ = Nothing

total
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                              (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                              _                     => Nothing
    getQuoted _           = Nothing
parsePrefix SChar input = getChar (unpack input)
  where
    getChar (c :: rest) = Just (c, ltrim (pack rest))
    getChar _           = Nothing
parsePrefix SInt input = case span isDigit input of
                           ("", rest)  => Nothing
                           (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemaL .+. schemaR) input = do
  (lVal, rest) <- parsePrefix schemaL input
  (rVal, rest') <- parsePrefix schemaR rest
  pure ((lVal, rVal), rest')

total
parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = 
  case parsePrefix schema input of
    Just (res, "") => Just res
    Just _         => Nothing
    Nothing        => Nothing

total
parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "schema" str = do schemaOk <- parseSchema (words str)
                                      pure $ SetSchema schemaOk
parseCommand schema "add" str    = do item <- parseBySchema schema str
                                      pure $ Add item
parseCommand schema "get" ""     = Just GetAll
parseCommand schema "get" val    = case all isDigit (unpack val) of
                                        False => Nothing
                                        True => Just (Get (cast val))
parseCommand schema "quit" ""    = Just Quit
parseCommand _ _ _               = Nothing

total
parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
  (cmd, args) => parseCommand schema cmd (ltrim args)

total
display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SChar} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr)
        = "(" ++ display iteml ++ ", " ++ display itemr ++ ")"

total
getEntry : (id : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry id store@(MkData schema size items) =
  case integerToFin id size of
    Nothing => Just ("Out of range\n", store)
    (Just id) => Just $ (display (index id items) ++ "\n", store)

total
setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z   => Just (MkData schema _ [])
                              S k => Nothing

total
getAll : (store : DataStore) -> String
getAll store = concatMap (\(id, item) => show id ++ ": " ++ display item ++ "\n") $
                 zipWithIndex (toList (items store))
  where
    zipWithIndex : List a -> List (Nat, a)
    zipWithIndex l = zip [0 .. length l] l

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (SetSchema newSchema) => case setSchema store newSchema of
                                     Nothing => Just ("Can't update schema\n", store)
                                     Just newStore => Just ("OK\n", newStore)
    Just (Add item) => Just ("ID" ++ show (size store) ++ "\n",
                             addToStore store item)
    Just (Get id)   => getEntry id store
    Just GetAll     => Just (getAll store, store)
    Just Quit       => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
