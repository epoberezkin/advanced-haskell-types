module Basic1 where

import ClassyPrelude

data AccountInfo = AccountInfo
                    { name :: Text
                    , displayName :: Text }

data Account = User AccountInfo | Org AccountInfo Members
type Members = [Account] -- but we only want users here, not orgs...

accountName :: Account -> Text
accountName (User info) = name info
accountName (Org info _) = name info

-- and we also have to report errors in functions that only work on certain type of accounts

orgMembers :: Account -> Maybe Members
orgMembers (User _) = Nothing
orgMembers (Org _ ms) = Just ms

-- but we can have list of mixed account types

type Accounts = [Account]
