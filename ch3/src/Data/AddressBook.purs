module Data.AddressBook where

import Prelude
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry =
  entry.lastName  <> ", " <>
  entry.firstName <> ": " <>
  showAddress entry.address

showAddress :: Address -> String
showAddress address =
  address.street <> ", " <>
  address.city   <> ", " <>
  address.state

insertEntry :: Entry -> (AddressBook -> AddressBook)
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry field value = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry
      | field == "name" = value == fullName entry
      | otherwise       = value == entry.address.street

fullName :: Entry -> String
fullName entry = entry.firstName <> " " <> entry.lastName

hasEntryName :: String -> AddressBook -> Boolean
hasEntryName name book = not null matches
  where
    matches :: AddressBook
    matches = filter (\entry -> fullName entry == name) book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy (\a b -> fullName a == fullName b) book

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry field value book = map showEntry $ findEntry field value book
