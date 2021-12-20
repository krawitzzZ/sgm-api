module Domain.Password
  ( Password(..)
  , PasswordHash(..)
  , mkPassword
  , hashPassword
  , checkPassword
  ) where

import           Control.Exception.Safe                   ( MonadThrow
                                                          , throwM
                                                          )
import           Data.Aeson                               ( FromJSON(..)
                                                          , Options(..)
                                                          , defaultOptions
                                                          , genericParseJSON
                                                          )
import qualified Data.Password.Argon2                    as P
import           Data.Password.Instances                  ( )
import           Domain.Exception                         ( DomainException(InvalidPassword) )
import           RIO                                      ( ($)
                                                          , (<&>)
                                                          , Bool(..)
                                                          , Eq
                                                          , Generic
                                                          , MonadIO
                                                          , Read
                                                          , Show
                                                          , Text
                                                          , return
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

checkPassword :: (MonadThrow m) => Password -> PasswordHash -> m ()
checkPassword (Password pwd) (PasswordHash pwdHash) = case P.checkPassword pwd pwdHash of
  P.PasswordCheckSuccess -> return ()
  P.PasswordCheckFail    -> throwM InvalidPassword
