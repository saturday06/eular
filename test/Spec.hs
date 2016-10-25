{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Spec where

import           ClassyPrelude
import           Test.Hspec

spec :: Spec
spec = do
  describe "++" $ do
    it "works!" $ do
      "imm" `shouldBe` "imma"
