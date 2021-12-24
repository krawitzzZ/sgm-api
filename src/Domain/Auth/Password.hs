{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Auth.Password
  ( Password(..)
  , PasswordHash(..)
  , mkPassword
  , hashPassword
  , checkPassword
  , validatePassword
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
import qualified Data.Password.Validate                  as PV
import           Data.Password.Validate                   ( ValidPasswordPolicy )
import           Data.String.Conversions                  ( cs )
import           Data.Validity                            ( Validity(..)
                                                          , valid
                                                          )
import           Data.Validity.Text                       ( )
import           Domain.Exception                         ( DomainException(..) )
import           RIO                                      ( ($)
                                                          , (<&>)
                                                          , Bool(..)
                                                          , Eq
                                                          , Generic
                                                          , MonadIO
                                                          , Read
                                                          , Show
                                                          , Text
                                                          , const
                                                          , return
                                                          , show
                                                          )


newtype Password = Password { unPassword :: P.Password }
  deriving (Show, Generic)

-- Orphan instance for validation purposes
-- Password will be validated separately using `validatePassword`
instance Validity P.Password where
  validate = const valid
instance Validity Password where
  validate = const valid

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
  P.PasswordCheckFail    -> throwM $ InvalidPassword "Provided password is invalid"

validatePassword :: (MonadThrow m) => Password -> ValidPasswordPolicy -> m ()
validatePassword (Password pwd) policy = case PV.validatePassword policy pwd of
  PV.ValidPassword           -> return ()
  PV.InvalidPassword reasons -> throwM $ InvalidPassword $ cs (show reasons)
