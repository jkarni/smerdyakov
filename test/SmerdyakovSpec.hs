module SmerdyakovSpec (spec) where

import Smerdyakov
import Smerdyakov.System
import Test.Hspec

spec :: Spec
spec = do

  describe "sudo" $ do

    it "fails if there is no sudo" $ do
      interpretIO (give :: Action Sudo) `shouldThrow`
        \e -> e == ExpectationFailure "Need sudo"

  describe "Action" $ do

    it "allows throwing exceptions" $ do
      let e = ExpectationFailure "hi"
      interpretIO (throwA e) `shouldThrow` anyException
