module Main where

import ClassyPrelude

import TypeFamilies4

main :: IO ()
main = do
  let john = AUser (AccountInfo "john" "John Doe")
      team = AOrg (AccountInfo "team" "John's team") [john]
      accounts = [User' john, Org' team]
  putStrLn $ accountName john
  putStrLn $ accountName team
  print . map accountName $ orgMembers team
  print $ map accountName' accounts      
      