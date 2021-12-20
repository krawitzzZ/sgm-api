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
import           Domain.Password                          ( Password
                                                          , checkPassword
                                                          )
import           Domain.User                              ( User(..)
                                                          , UserData
                                                          )
import           RIO                                      ( (<&>)
                                                          , (>>)
                                                          , (>>=)
                                                          , MonadIO
                                                          , MonadReader
                                                          , Text
                                                          )
import           Servant.Auth.Server                      ( JWTSettings )


loginUser
  :: (Has JWTSettings e, MonadReader e m, UserRepository m, MonadThrow m, MonadTime m, MonadIO m)
  => Text
  -> Password
  -> m JWT
loginUser name pwd =
  getUserByName name >>= \u -> checkPassword pwd (userPassword u) >> mkJwt (mkAuthenticatedUser u)

refreshJwtToken
  :: (Has JWTSettings e, MonadReader e m, MonadTime m, MonadIO m) => AuthenticatedUser -> m JWT
refreshJwtToken = mkJwt

-- TODO validate password if strong enough
signupUser
  :: (Has JWTSettings e, MonadReader e m, UserRepository m, MonadTime m, MonadIO m)
  => UserData
  -> m (User, JWT)
signupUser userData = createUser userData >>= \u -> mkJwt (mkAuthenticatedUser u) <&> (u, )
