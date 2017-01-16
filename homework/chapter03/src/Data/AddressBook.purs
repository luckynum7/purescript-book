module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

-- TODO: Test if the function works
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy filterEntry
  where
    filterEntry ::Entry -> Entry -> Boolean
    filterEntry entry1 entry2 = entry1.firstName == entry2.firstName &&
                                entry1.lastName == entry2.lastName
