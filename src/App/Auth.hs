module App.Auth
  ( signupUser
  , loginUser
  , refreshJwtToken
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Control.Monad.Reader.Has                 ( Has )
import           Control.Monad.Time                       ( MonadTime )
import           Domain.Auth                              ( AuthenticatedUser(..)
                                                          , JWT(..)
                                                          , mkAuthenticatedUser
                                                          , mkJwt
                                                          )
import           Domain.Class                             ( UserRepository(..) )
import           Domain.Env                               ( Env )
import           Domain.Password                          ( Password
                                                          , checkPassword
                                                          )
import           Domain.User                              ( NewUserData
                                                          , User(..)
                                                          )
import           RIO                                      ( (<&>)
                                                          , (>>)
                                                          , (>>=)
                                                          , MonadIO
                                                          , MonadReader
                                                          , Text
                                                          )


loginUser
  :: (Has Env e, MonadReader e m, UserRepository m, MonadThrow m, MonadTime m, MonadIO m)
  => Text
  -> Password
  -> m JWT
loginUser username pwd = getUserByUsername username
  >>= \u -> checkPassword pwd (uPassword u) >> mkJwt (mkAuthenticatedUser u)

refreshJwtToken
  :: (Has Env e, MonadReader e m, MonadTime m, MonadIO m) => AuthenticatedUser -> m JWT
refreshJwtToken = mkJwt

-- TODO validate password if strong enough
signupUser
  :: (Has Env e, MonadReader e m, UserRepository m, MonadTime m, MonadIO m)
  => NewUserData
  -> m (User, JWT)
signupUser userData = createUser userData >>= \u -> mkJwt (mkAuthenticatedUser u) <&> (u, )
