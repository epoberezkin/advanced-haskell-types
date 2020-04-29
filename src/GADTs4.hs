{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module GADTs4 where

import ClassyPrelude

data AInfo = AInfo
              { name :: Text
              , displayName :: Text }

data AType = AUser | AOrg

-- (a :: AType) requires KindSignatures and it makes this requirement explicit
data Account (a :: AType) where
  User :: AInfo -> Account 'AUser
  Org :: AInfo -> Members -> Account 'AOrg

type Members = [Account 'AUser] -- now we can have only users in the list of members

accountName :: Account a -> Text
accountName (User info) = name info
accountName (Org info _) = name info

-- and we do not need to report errors as the function now accepts only orgs

orgMembers :: Account 'AOrg -> Members
orgMembers (Org _ ms) = ms

-- but we still cannot have a mixed list of users and orgs without creating a wrapper

type Accounts = [A]
data A = forall a. A (Account a)

main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [A user, A org]
  putStrLn $ accountName user
  putStrLn $ accountName org
  print . map accountName $ orgMembers org
  print $ map (\(A acc) -> accountName acc) accounts
