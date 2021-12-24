module App.Auth
  ( signupUser
  , loginUser
  , refreshJwtToken
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Domain.App.Class                         ( Authentication(..)
                                                          , UserRepository(..)
                                                          )
import           Domain.Auth                              ( JWT )
import           Domain.Auth.Password                     ( Password )
import           Domain.Auth.UserClaims                   ( UserClaims )
import           Domain.User                              ( User(..) )
import           Domain.User.UserData                     ( NewUserData )
import           RIO                                      ( (<&>)
                                                          , (>>)
                                                          , (>>=)
                                                          , Text
                                                          )


loginUser :: (UserRepository m, Authentication m, MonadThrow m) => Text -> Password -> m JWT
loginUser username pwd = getUserByUsername username >>= \u -> do
  checkPassword pwd (uPassword u) >> createJwt u

refreshJwtToken :: (Authentication m) => UserClaims -> m JWT
refreshJwtToken = refreshJwt

signupUser :: (Authentication m, UserRepository m) => NewUserData -> m (User, JWT)
signupUser userData = createUser userData >>= \u -> createJwt u <&> (u, )
