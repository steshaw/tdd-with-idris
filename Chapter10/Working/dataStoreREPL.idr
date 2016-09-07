module Main

import Data.Vect
import DataStore

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Integer -> Command schema
  GetAll    : Command schema
  Quit      : Command schema

mutual
  total
  parseSchema : List String -> Maybe Schema
  parseSchema ("String" :: xs) = parseRest SString xs
  parseSchema ("Char" :: xs)   = parseRest SChar xs
  parseSchema ("Int" :: xs)    = parseRest SInt xs
  parseSchema _                = Nothing

  total
  parseRest : Schema -> List String -> Maybe Schema
  parseRest schema xs = case xs of
                             [] => Just schema
                             _  => do schemaR <- parseSchema xs
                                      pure $ schema .+. schemaR

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
getEntry : (id : Integer) -> (store : DataStore schema) -> Maybe (String, DataStore schema)
getEntry id store@(MkData size items) =
  case integerToFin id size of
    Nothing => Just ("Out of range\n", store)
    (Just id) => Just $ (display (index id items) ++ "\n", store)

{-
total
setSchema : (store : DataStore schema) -> (newSchema : Schema) -> Maybe (DataStore newSchema)
setSchema store schema = case size store of
                              Z   => Just (MkData _ [])
                              S k => Nothing
-}

showItems : DataStore schema -> List (SchemaType schema)
showItems store with (storeView store)
  showItems store | SNil = []
  showItems (addToStore item store) | (SAdd rec) = item :: showItems store | rec

total
getAll : (store : DataStore schema) -> String
getAll store = concatMap (\(id, item) => show id ++ ": " ++ display item ++ "\n") $
                 zipWithIndex (showItems store)
  where
    zipWithIndex : List a -> List (Nat, a)
    zipWithIndex l = zip [0 .. length l] l

total
processInput : DataStore schema -> String -> Maybe (String, DataStore schema)
processInput store input {schema} =
  case parse schema input of
    Nothing => Just ("Invalid command\n", store)
    Just (SetSchema newSchema) => Just ("SetSchema not implemented\n", store)
{-
    Just (SetSchema newSchema) => case setSchema store newSchema of
                                     Nothing => Just ("Can't update schema\n", store)
                                     Just newStore => Just ("OK\n", newStore)
-}
    Just (Add item) => Just ("ID" ++ show (length (showItems store)) ++ "\n",
                             addToStore item store)
    Just (Get id)   => getEntry id store
    Just GetAll     => Just (getAll store, store)
    Just Quit       => Nothing

main : IO ()
main = replWith (empty {schema = SString}) "Command: " processInput
