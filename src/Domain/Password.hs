module Domain.Password
  ( Password(..)
  , PasswordHash(..)
  , mkPassword
  , hashPassword
  , isValid
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , Options(..)
                                                          , defaultOptions
                                                          , genericParseJSON
                                                          )
import qualified Data.Password.Argon2                    as P
import           Data.Password.Instances                  ( )
import           RIO                                      ( ($)
                                                          , (<&>)
                                                          , (==)
                                                          , Bool(..)
                                                          , Eq
                                                          , Generic
                                                          , MonadIO
                                                          , Read
                                                          , Show
                                                          , Text
                                                          )


newtype Password = Password { unPassword :: P.Password }
  deriving (Show, Generic)

instance FromJSON Password where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True }

newtype PasswordHash = PasswordHash { unPasswordHash :: P.PasswordHash P.Argon2 }
  deriving (Eq, Show, Read, Generic)

mkPassword :: Text -> Password
mkPassword pwd = Password $ P.mkPassword pwd

hashPassword :: (MonadIO m) => Password -> m PasswordHash
hashPassword (Password pwd) = P.hashPassword pwd <&> PasswordHash

isValid :: Password -> PasswordHash -> Bool
isValid (Password pwd) (PasswordHash pwdHash) =
  P.checkPassword pwd pwdHash == P.PasswordCheckSuccess
