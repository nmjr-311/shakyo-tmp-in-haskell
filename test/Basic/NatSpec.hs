module Basic.NatSpec where

import           Basic.Nat  (ReifyNat (reifyNat), S, Sum, Z)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec_reifyNat :: Spec
spec_reifyNat = describe "NatSpec" $ do
  primitives

primitives :: Spec
primitives = describe "Primitives" $ do
  it "reifyNat @Z shouldBe 0" $ reifyNat @Z `shouldBe` 0
  it "reifyNat @(S(S Z)) shouldBe 2" $ reifyNat @(S (S Z)) `shouldBe` 2

sum :: Spec
sum = describe "Sums" $ do
  it "reifyNat @(Sum Z Z) shouldBe 0" $ reifyNat @(Sum Z Z) `shouldBe` 0
  it "reifyNat @(Sum (S Z) Z) shouldBe 1" $ reifyNat @(Sum (S Z) Z) `shouldBe` 1
  it "reifyNat @(Sum (S Z) (S (S Z))) shouldBe 3" $ reifyNat @(Sum (S Z) (S (S Z))) `shouldBe` 3
