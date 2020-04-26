module Basic2 where

import ClassyPrelude

data AccountInfo = AccountInfo
                    { name :: Text
                    , displayName :: Text }

data User = User AccountInfo
data Org = Org AccountInfo Members
type Members = [User] -- this is better now!

-- we cannot write functions that work on both users and orgs though,
-- we can have 2 separate functions...

userName :: User -> Text
userName (User info) = name info

orgName :: Org -> Text
orgName (Org info _) = name info

-- or we can define a type class and interfaces

class AccountName a where
  accountName :: a -> Text
instance AccountName User where
  accountName (User info) = name info
instance AccountName Org where
  accountName (Org info _) = name info

-- we can have more precise type in orgMembers function and avoid runtime errors completely

orgMembers :: Org -> Members
orgMembers (Org _ ms) = ms

-- but if we want to have the list with both users and orgs we will have to create a wrapper type

type Accounts = [UserOrOrg]
data UserOrOrg = User' User | Org' Org
