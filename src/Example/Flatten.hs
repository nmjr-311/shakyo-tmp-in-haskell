{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Example.Flatten where

class Flatten a where
  flatten :: a -> [ElementOf a]

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten x = x

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten x = flatten (concat x)
