module Basic.Nat where

import           Numeric.Natural (Natural)

data Z
data S a

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)
