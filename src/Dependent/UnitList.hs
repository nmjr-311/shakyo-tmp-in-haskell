module Dependent.UnitList where

import           Dependent.HList (HList (..))

class UnitList as where
  unitList :: HList as

instance UnitList '[] where
  unitList = HNil

instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = () ::: unitList
