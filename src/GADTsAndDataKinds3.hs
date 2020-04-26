{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module GADTsAndDataKinds3 where

import ClassyPrelude

data AccountInfo = AccountInfo
                    { name :: Text
                    , displayName :: Text }

data AccountType = User | Org

data Account (a :: AccountType) where -- AccountType here requires KindSignatures but it is implied anyway
  AUser :: AccountInfo -> Account 'User
  AOrg :: AccountInfo -> Members -> Account 'Org


type Members = [Account 'User] -- now we can have only users in the list of members

accountName :: Account a -> Text
accountName (AUser info) = name info
accountName (AOrg info _) = name info

-- and we do not need to report errors as the function now accepts only orgs

orgMembers :: Account 'Org -> Members
orgMembers (AOrg _ ms) = ms

-- but we still cannot have a mixed list of users and orgs without creating a wrapper

-- type Accounts = [Account] -- this fails

type Accounts = [UserOrOrg]

data UserOrOrg = User' (Account 'User) | Org' (Account 'Org)
accountName' :: UserOrOrg -> Text
accountName' (User' a) = accountName a
accountName' (Org' a) = accountName a
