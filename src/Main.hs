module Main where

import ClassyPrelude

import qualified Basic1
import qualified ExistentialQuantification2
import qualified DataFamilies3
import qualified GADTs4

main :: IO ()
main = do
  Basic1.main
  ExistentialQuantification2.main
  DataFamilies3.main
  GADTs4.main
