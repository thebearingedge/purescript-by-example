module Main where

import Prelude
import Control.Plus (empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, insertEntry, hasEntryName, printEntry)

tim :: Entry
tim =
  { firstName: "Tim"
  , lastName:  "Davis"
  , address: { street: "1234"
             , city:   "Huntington Beach"
             , state:  "CA"
             }
  }

p48 :: Entry
p48 =
  { firstName: "P48V"
  , lastName: "Corporation"
  , address: { street: "5678"
             , city:   "Costa Mesa"
             , state:  "CA"
             }
  }

emptyBook :: AddressBook
emptyBook = empty

addresses :: AddressBook
addresses = insertEntry tim $ insertEntry p48 $ insertEntry tim emptyBook

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ hasEntryName "Tim Davis" addresses
  logShow $ printEntry "name" "Tim Davis" addresses
  logShow $ printEntry "street" "5678" addresses
