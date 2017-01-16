module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry, findEntryByStreet)
import Data.List (null)
import Data.Maybe (Maybe)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

printEntryByStreet :: String -> AddressBook -> Maybe String
printEntryByStreet street book = showEntry <$> findEntryByStreet street book

main :: Eff (console :: CONSOLE) Unit
main = do
  let book1 = insertEntry example emptyBook

  log "@findEntry"
  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  log "\n@findEntryByStreet"
  logShow $ printEntryByStreet "123 Fake St." book0
  logShow $ printEntryByStreet "123 Fake St." book1
  log "\n@Data.List.null"
  logShow $ null book0
  logShow $ null book1
