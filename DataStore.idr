module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) -> 
              DataStore

size : DataStore -> Nat                            
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs
    

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size                              
parseCommand "quit" "" = Just Quit
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
getEntry pos store input = let store_items = items store in
                               case integerToFin pos (size store) of
                                    Nothing => Just ("Out of range\n", store)
                                    Just id => Just (index id store_items ++ "\n", store)                                    

searchItemsWithString : (items : Vect n String) -> (pos: Nat) -> (str : String) -> String
searchItemsWithString [] pos str = ""
searchItemsWithString (x :: xs) pos str = case isInfixOf str x of
                                           True => (show pos) ++ ": " ++ x ++ "\n" ++ searchItemsWithString xs (S pos) str
                                           False => searchItemsWithString xs (S pos) str                                          

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp  of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => getEntry pos store inp
                              Just (Search str) => Just (searchItemsWithString (items store) Z str, store)
                              Just Size => Just (show (size store) ++ "\n", store)
                              Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
