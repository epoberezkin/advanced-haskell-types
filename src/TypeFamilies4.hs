{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


module TypeFamilies4 where

import ClassyPrelude

data AccountInfo = AccountInfo
                    { name :: Text
                    , displayName :: Text }

data AccountType = User | Org

data family Account (a :: AccountType)
data instance Account 'User = AUser AccountInfo
data instance Account 'Org = AOrg AccountInfo Members

type Members = [Account 'User] -- now we can have only users in the list of members

class AccountName a where
  accountName :: Account a -> Text
instance AccountName 'User where
  accountName (AUser info) = name info
instance AccountName 'Org where
  accountName (AOrg info _) = name info


orgMembers :: Account 'Org -> Members
orgMembers (AOrg _ ms) = ms

-- but we still cannot have a mixed list of users and orgs without creating a wrapper

-- type Accounts = [Account] -- this fails

type Accounts = [UserOrOrg]

data UserOrOrg = User' (Account 'User) | Org' (Account 'Org)
accountName' :: UserOrOrg -> Text
accountName' (User' a) = accountName a
accountName' (Org' a) = accountName a

