module Basic.NatSpec where

import           Basic.Nat  (ReifyNat (reifyNat), S, Z)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec_reifyNat :: Spec
spec_reifyNat = describe "NatSpec" $ do
  it "reifyNat @Z shouldBe 0" $ reifyNat @Z `shouldBe` 0
  it "reifyNat @(S(S Z)) shouldBe 2" $ reifyNat @(S (S Z)) `shouldBe` 2
