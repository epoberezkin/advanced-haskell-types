module Basic1 where

import ClassyPrelude

data AInfo = AInfo
              { name :: Text
              , displayName :: Text }

data Account = User AInfo | Org AInfo Members
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


main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [user, org]
  putStrLn $ accountName user
  putStrLn $ accountName org
  case orgMembers org of
    Just ms -> print $ map accountName ms
    Nothing -> return ()
  print $ map accountName accounts
