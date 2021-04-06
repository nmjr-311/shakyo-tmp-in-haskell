{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Dependent.HList (HList(..), head, append) where

import           Basic.TypeOf (TypeOf (typeOf))
import           Data.Kind    (Type)
import           Prelude      hiding (head)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:::) :: a -> HList as-> HList (a ': as)

infixr 5 :::

instance TypeOf (HList '[]) where
  typeOf = "[]"

instance (TypeOf a, TypeOf (HList as)) => TypeOf (HList (a ': as)) where
  typeOf = typeOf @a ++ " : " ++ typeOf @(HList as)

type family (as :: [Type]) ++ (bs :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

head :: HList (a ': as) -> a
head (a ::: _) = a

append :: HList as -> HList bs -> HList (as ++ bs)
append HNil bs       = bs
append (a ::: as) bs = a ::: append as bs
