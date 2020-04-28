{-# LANGUAGE ExistentialQuantification #-}

module ExistentialQuantification2 where

import ClassyPrelude

data AInfo = AInfo
              { name :: Text
              , displayName :: Text }

newtype User = User AInfo
data Org = Org AInfo Members
type Members = [User] -- this is better now!

-- we cannot write functions that work on both users and orgs though,
-- we can have 2 separate functions...

userName :: User -> Text
userName (User info) = name info

orgName :: Org -> Text
orgName (Org info _) = name info

-- or we can define a type class and interfaces - it looks better

class Account a where
  accountName :: a -> Text
instance Account User where
  accountName (User info) = name info
instance Account Org where
  accountName (Org info _) = name info

-- we can have more precise type in orgMembers function and avoid runtime errors completely

orgMembers :: Org -> Members
orgMembers (Org _ ms) = ms

-- but if we want to have the list with both users and orgs we will have to create a wrapper type

type Accounts = [A]
data A = forall a. Account a => A a


main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [A user, A org]
  putStrLn $ accountName user
  putStrLn $ accountName org
  print . map accountName $ orgMembers org
  print $ map (\(A acc) -> accountName acc) accounts
