{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Turncoat (module Turncoat) where

import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Sqlite

type role PlatformT nominal
type PlatformT :: (Type -> Type) -> Type
data PlatformT f = MkPlatform
  { id :: C f Int32
  , name :: C f Text
  , lastSyncTime :: C f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Platform :: Type
type Platform = PlatformT Identity

type PlatformId :: Type
type PlatformId = PrimaryKey PlatformT Identity

deriving stock instance Show Platform
deriving stock instance Show PlatformId

instance Table PlatformT where
  data PrimaryKey PlatformT f = MkPlatformId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkPlatformId . (.id)

type role AccountT nominal
type AccountT :: (Type -> Type) -> Type
data AccountT f = MkAccount
  { id :: C f Int32
  , platformId :: PrimaryKey PlatformT f
  , username :: C f Text
  , accessToken :: C f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Account :: Type
type Account = AccountT Identity

type AccountId :: Type
type AccountId = PrimaryKey AccountT Identity

deriving stock instance Show Account
deriving stock instance Show AccountId

instance Table AccountT where
  data PrimaryKey AccountT f = MkAccountId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkAccountId . (.id)

type role FollowerT nominal
type FollowerT :: (Type -> Type) -> Type
data FollowerT f = MkFollower
  { id :: C f Int32
  , accountId :: PrimaryKey AccountT f
  , username :: C f Text
  , firstSeen :: C f UTCTime
  , lastSeen :: C f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Follower :: Type
type Follower = FollowerT Identity

type FollowerId :: Type
type FollowerId = PrimaryKey FollowerT Identity

deriving stock instance Show Follower
deriving stock instance Show FollowerId

instance Table FollowerT where
  data PrimaryKey FollowerT f = MkFollowerId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkFollowerId . (.id)

type role UnfollowT nominal
type UnfollowT :: (Type -> Type) -> Type
data UnfollowT f = MkUnfollow
  { id :: C f Int32
  , followerId :: PrimaryKey FollowerT f
  , unfollowedAt :: C f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Unfollow :: Type
type Unfollow = UnfollowT Identity

type UnfollowId :: Type
type UnfollowId = PrimaryKey UnfollowT Identity

deriving stock instance Show Unfollow
deriving stock instance Show UnfollowId

instance Table UnfollowT where
  data PrimaryKey UnfollowT f = MkUnfollowId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MkUnfollowId . (.id)

type role TurncoatDb representational
type TurncoatDb :: (Type -> Type) -> Type
data TurncoatDb f = MkTurncoatDb
  { platforms :: f (TableEntity PlatformT)
  , accounts :: f (TableEntity AccountT)
  , followers :: f (TableEntity FollowerT)
  , unfollows :: f (TableEntity UnfollowT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

migrations :: CheckedDatabaseSettings Sqlite TurncoatDb
migrations =
  defaultMigratableDbSettings
    `withDbModification` dbModification
      { platforms = modifyCheckedTable id checkedTableModification{lastSyncTime = "last_sync_time"}
      , accounts =
          modifyCheckedTable
            id
            checkedTableModification
              { platformId = MkPlatformId "platform_id"
              , accessToken = "access_token"
              }
      , followers =
          modifyCheckedTable
            id
            checkedTableModification
              { firstSeen = "first_seen"
              , lastSeen = "last_seen"
              , accountId = MkAccountId "account_id"
              }
      , unfollows =
          modifyCheckedTable
            id
            checkedTableModification
              { followerId = MkFollowerId "follow_id"
              , unfollowedAt = "unfollowed_at"
              }
      }

turncoatDb :: DatabaseSettings Sqlite TurncoatDb
turncoatDb = unCheckDatabase migrations

platforms :: DatabaseEntity Sqlite TurncoatDb (TableEntity PlatformT)
platforms = turncoatDb.platforms

accounts :: DatabaseEntity Sqlite TurncoatDb (TableEntity AccountT)
accounts = turncoatDb.accounts

followers :: DatabaseEntity Sqlite TurncoatDb (TableEntity FollowerT)
followers = turncoatDb.followers

unfollows :: DatabaseEntity Sqlite TurncoatDb (TableEntity UnfollowT)
unfollows = turncoatDb.unfollows

instance HasDefaultSqlDataType Sqlite UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing False
