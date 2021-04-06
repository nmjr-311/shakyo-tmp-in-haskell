module Generic.Authentication where


data Authentication
  = AuthBasic Username Password
  | AuthSSH PublicKey

type Username = String
type Password = String
type PublicKey = String

class Generic a where
  type Rep a
  genericize :: a -> Rep a

instance Generic Authentication where
  type Rep Authentication = Either (Username, Password) PublicKey
  genericize (AuthBasic user pass) = Left (user, pass)
  genericize (AuthSSH key)         = Right key
