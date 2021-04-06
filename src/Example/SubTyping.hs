{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

{-# LANGUAGE FlexibleContexts      #-}
module Example.SubTyping where

import           Data.Kind          (Type)
import           Data.Type.Equality ((:~:) (Refl))

data MyKind = Both | Input | Output

data MyType :: MyKind -> Type where
  TInt :: MyType 'Both
  TInputObject :: MyType 'Input
  TOutputObject :: MyType 'Output

data SubKind :: MyKind -> MyKind -> Type where
  KRefl :: SubKind k k
  KBoth :: SubKind 'Both k

class IsSubKind k1 k2 where
  subKindProof :: SubKind k1 k2

instance IsSubKind 'Both k where
  subKindProof = KBoth

instance (k ~ 'Input) => IsSubKind 'Input k where
  subKindProof = KRefl

instance (k ~ 'Output) => IsSubKind 'Output k where
  subKindProof = KRefl

newtype InputValue = InputValue ()
newtype OutputValue = OutputValue ()

type family ParserInput (k :: MyKind) where
  ParserInput 'Both = InputValue
  ParserInput 'Input = InputValue
  ParserInput 'Output = OutputValue

inputParserInput :: forall k. IsSubKind k 'Input => ParserInput k :~: InputValue
inputParserInput = case subKindProof @k @'Input of
  KRefl -> Refl
  KBoth -> Refl

someFunction :: forall k. IsSubKind k 'Input => ParserInput k
someFunction = case inputParserInput @k of
  Refl -> InputValue () -- since ParserInput k :~: InputValue for any k in this clause
