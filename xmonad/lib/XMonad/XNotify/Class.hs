{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XMonad.XNotify.Class where

import Data.Monoid
import Data.Typeable
import Data.String (IsString)

class (Typeable t,Ord t) => TokenSelector t

data TkFresh = TkFresh deriving (Eq,Ord)
instance TokenSelector TkFresh

newtype Icon = Icon { iconPath :: String }
  deriving (Eq,Ord,Show,IsString)

class Monoid n => IsNotification n where
  summary :: String -> n
  body :: String -> n
  icon :: Icon -> n

class (IsNotification n,Monad m) => HasNotifications n m | m -> n where
  notify :: TokenSelector t => t -> n -> m ()
  freshNotify :: n -> m ()
  freshNotify = notify TkFresh

mkBody :: IsNotification n => String -> String -> String -> n
mkBody name col v = body
  $ name <> ": <span foreground = \""
  <> col <> "\">" <> v <> "</span>"


