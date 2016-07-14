module SmerdyakovSpec (spec) where

import Smerdyakov
import Smerdyakov.System
import Control.Monad.Trans.Except
import Test.Hspec


spec :: Spec
spec = describe "sudo" $ do

  it "fails if there is no sudo" $ do
    runExceptT (interpretIO give) `shouldReturn`
      (Left (ExpectationFailure "Need sudo") :: Either ActionError Sudo)
