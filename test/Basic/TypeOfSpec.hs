{-# LANGUAGE TypeApplications #-}
module Basic.TypeOfSpec where

import           Basic.TypeOf (TypeOf, typeOf)
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "TypeOfSpec" $ do
  basic
  composition
  userDefined

basic :: Spec
basic = describe "Basic" $ do
  it "typeOf @Char shouldBe Char" $ typeOf @Char `shouldBe` "Char"
  it "typeOf @Bool shouldBe Bool" $ typeOf @Bool `shouldBe` "Bool"

composition :: Spec
composition = describe "Composition" $ do
  it "typeOf @(Bool, Char) shouldBe (Bool, Char)" $ typeOf @(Bool, Char) `shouldBe` "(Bool, Char)"
  it "typeOf @[Char] shouldBe [Char]" $ typeOf @[Char] `shouldBe` "[Char]"
  it "typeOf @[(Bool, Char)] shouldBe [(Bool, Char)]" $ typeOf @[(Bool, Char)] `shouldBe` "[(Bool, Char)]"

userDefined :: Spec
userDefined = describe "UserDefinedTypes" $
  it "typeOf @Person shouldBe Person" $ typeOf @Person `shouldBe` "Person"

data Person = MkPerson { name :: String, age :: Int }

instance TypeOf Person where
  typeOf = "Person"
