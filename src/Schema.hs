{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Text (Text)
import Database.Beam

data UserT f = User
  { _userEmail :: Columnar f Text,
    _userFirstName :: Columnar f Text,
    _userLastName :: Columnar f Text,
    _userPassword :: Columnar f Text
  }
  deriving (Generic)

instance Beamable UserT

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Beamable, Generic)
  primaryKey = UserId . _userEmail
