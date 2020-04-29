{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


module DataFamilies3 where

import ClassyPrelude

data AInfo = AInfo
              { name :: Text
              , displayName :: Text }

data AType = AUser | AOrg

data family Account (a :: AType)
data instance Account 'AUser = User AInfo
data instance Account 'AOrg = Org AInfo Members

type Members = [Account 'AUser] -- now we can have only users in the list of members

class Acc (a :: AType) where
  accountName :: Account a -> Text
instance Acc 'AUser where
  accountName (User info) = name info
instance Acc 'AOrg where
  accountName (Org info _) = name info

orgMembers :: Account 'AOrg -> Members
orgMembers (Org _ ms) = ms

-- but we still cannot have a mixed list of users and orgs without creating a wrapper
-- type Accounts = [Account] -- this fails

type Accounts = [A]

data A = forall a. Acc a => A (Account a)

main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [A user, A org]
  putStrLn $ accountName user
  putStrLn $ accountName org
  print . map accountName $ orgMembers org
  print $ map (\(A acc) -> accountName acc) accounts
