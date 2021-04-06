module Dependent.HListSpec where

import           Basic.TypeOf    (typeOf)
import           Dependent.HList (HList (..))
import           Test.Hspec      (Spec, describe, it, shouldBe)

spec_HList :: Spec
spec_HList = describe "HListSpec" $ do
  construction

construction :: Spec
construction = describe "Construction" $ do
  it "typeOf @(HList (Int : Bool : Char : [])) shouldBe Int : Bool : Char : []"
    $ typeOf @(HList (Int ': Bool ': Char ': '[])) `shouldBe` "Int : Bool : Char : []"
