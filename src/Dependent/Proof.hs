{-# LANGUAGE FlexibleInstances #-}
module Dependent.Proof where

import           Data.Kind       (Type)
import           Dependent.HList (HList (..))

data OneToThree :: Type -> Type -> Type -> [Type] -> Type where
  One :: OneToThree a b c '[a]
  Two :: OneToThree a b c '[a, b]
  Three :: OneToThree a b c '[a, b, c]

sumUpToThree :: OneToThree Int Int Int as -> HList as -> Int
sumUpToThree One (a ::: HNil)               = a
sumUpToThree Two (a ::: b ::: HNil)         = a + b
sumUpToThree Three (a ::: b ::: c ::: HNil) = a + b + c

data Even as where
  EvenBase :: Even '[]
  EvenInd :: Even as -> Even (a ': b ': as)

type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': as) = (a, b) ': PairUp as

pairUp :: Even as -> HList as -> HList (PairUp as)
pairUp EvenBase HNil                 = HNil
pairUp (EvenInd ih) (a ::: b ::: as) = (a, b) ::: pairUp ih as

class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenBase

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenInd evenProof

pairUp' :: IsEven as => HList as -> HList (PairUp as)
pairUp' = pairUp evenProof
