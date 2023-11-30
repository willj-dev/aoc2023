module Day01Spec where

import Test.Hspec

spec :: Spec
spec = do
  it "should know that equality is reflexive" $ do
    2 `shouldBe` 2