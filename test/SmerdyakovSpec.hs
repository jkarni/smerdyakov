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

  describe "make" $ do

    it "provides parameters" $
      interpretIO (make (Proxy :: Proxy String) $ make (Proxy :: Proxy Int) needy)
        `shouldReturn` True

instance Gives Int where
  give = return 5

instance Gives String where
  give = return "5"



needy :: (Needs Int, Needs String) => Action Bool
needy = (\i s -> read s == (i :: Int)) <$> give <*> give
