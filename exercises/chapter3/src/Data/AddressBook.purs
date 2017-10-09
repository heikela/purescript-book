module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null)
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

printEntries = map showEntry

nameFilter :: String -> String -> Entry -> Boolean
nameFilter firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findEntryWithFilter :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
findEntryWithFilter filterFunc = head <<< filter filterFunc

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = findEntryWithFilter (nameFilter firstName lastName)

findEntryByStreetAddress :: String -> AddressBook -> Maybe Entry
findEntryByStreetAddress streetAddress =
  findEntryWithFilter \entry -> entry.address.street == streetAddress

isPresentWithFilter :: (Entry -> Boolean) -> AddressBook -> Boolean
isPresentWithFilter filterFunc = not <<< null <<< filter filterFunc

isNamePresent :: String -> String -> AddressBook -> Boolean
isNamePresent firstName lastName = isPresentWithFilter (nameFilter firstName lastName)
