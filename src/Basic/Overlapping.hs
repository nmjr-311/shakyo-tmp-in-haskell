{-# LANGUAGE FlexibleInstances #-}
module Basic.Overlapping where

class IsUnit a where
  isUnit :: Bool

instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False

guardUnit :: forall a. IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
  True -> Left "unit!"
  _    -> Right x
