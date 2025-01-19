{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Turncoat (module Turncoat) where

import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text, unpack)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types
import Database.Beam.Migrate
import Database.Beam.Sqlite

type Platform :: Type
data Platform
  = GitHub
  | Twitter
  deriving stock (Show, Eq)

type Action :: Type
data Action
  = Follow
  | Unfollow
  deriving stock (Show, Eq)

type role UserT nominal
type UserT :: (Type -> Type) -> Type
data UserT f = MkUser
  { id :: C f Int32
  , platform :: C f Platform
  , platformId :: C f Text
  , platformUsername :: C f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type User :: Type
type User = UserT Identity

type UserId :: Type
type UserId = PrimaryKey UserT Identity

deriving stock instance Show User
deriving stock instance Show UserId

instance Table UserT where
  data PrimaryKey UserT f = MkUserId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkUserId . (.id)

type role EventT nominal
type EventT :: (Type -> Type) -> Type
data EventT f = MkEvent
  { id :: C f Int32
  , source :: PrimaryKey UserT f
  , target :: PrimaryKey UserT f
  , action :: C f Action
  , timestamp :: C f LocalTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Event :: Type
type Event = EventT Identity

type EventId :: Type
type EventId = PrimaryKey EventT Identity

deriving stock instance Show Event
deriving stock instance Show EventId

instance Table EventT where
  data PrimaryKey EventT f = MkEventId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkEventId . (.id)

type role TurncoatDb representational
type TurncoatDb :: (Type -> Type) -> Type
data TurncoatDb f = MkTurncoatDb
  { users :: f (TableEntity UserT)
  , events :: f (TableEntity EventT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be Platform where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Platform where
  fromBackendRow =
    fromBackendRow >>= \case
      "GitHub" -> pure GitHub
      "Twitter" -> pure Twitter
      val -> fail ("Invalid value for Platform: " ++ unpack val)

instance HasDefaultSqlDataType Sqlite Platform where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be Action where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Action where
  fromBackendRow =
    fromBackendRow >>= \case
      "Follow" -> pure Follow
      "Unfollow" -> pure Unfollow
      val -> fail ("Invalid value for Platform: " ++ unpack val)

instance HasDefaultSqlDataType Sqlite Action where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

migrations :: CheckedDatabaseSettings Sqlite TurncoatDb
migrations =
  defaultMigratableDbSettings
    `withDbModification` dbModification
      { users =
          modifyCheckedTable
            id
            checkedTableModification
              { platformId = "platform_id"
              , platformUsername = "platform_username"
              }
      }

turncoatDb :: DatabaseSettings Sqlite TurncoatDb
turncoatDb = unCheckDatabase migrations

users :: DatabaseEntity Sqlite TurncoatDb (TableEntity UserT)
events :: DatabaseEntity Sqlite TurncoatDb (TableEntity EventT)
MkTurncoatDb{users, events} = turncoatDb
