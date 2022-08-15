module XMonad.XNotify
  ( module XMonad.XNotify.Class )
where

import XMonad.XNotify.Class
import Data.Monoid


mkBody :: IsNotification n => String -> String -> String -> n
mkBody name col v = body
  $ name <> ": <span foreground = \""
  <> col <> "\">" <> v <> "</span>"

