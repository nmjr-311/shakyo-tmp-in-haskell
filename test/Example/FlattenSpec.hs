module Example.FlattenSpec where

import           Example.Flatten (Flatten (flatten))
import           Test.Hspec      (Spec, describe, it, shouldBe)

spec_flatten :: Spec
spec_flatten = describe "FlattenSpec" $ do
  it "flatten [1, 2] shouldBe [1, 2]" $ flatten [1 :: Int, 2] `shouldBe` [1, 2]
  it "flatten [[1, 2], [3, 4]] shouldBe [1, 2, 3, 4]" $ flatten [[1 :: Int, 2], [3, 4]] `shouldBe` [1, 2, 3, 4]
