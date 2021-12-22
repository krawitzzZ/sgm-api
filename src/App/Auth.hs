module App.Auth
  ( signupUser
  , loginUser
  , refreshJwtToken
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Domain.Auth                              ( AuthUser(..)
                                                          , JWT(..)
                                                          , mkAuthUser
                                                          )
import           Domain.Class                             ( Authentication(..)
                                                          , UserRepository(..)
                                                          )
import           Domain.Password                          ( Password )
import           Domain.User                              ( NewUserData
                                                          , User(..)
                                                          )
import           RIO                                      ( (<&>)
                                                          , (>>)
                                                          , (>>=)
                                                          , Text
                                                          )


loginUser :: (UserRepository m, Authentication m, MonadThrow m) => Text -> Password -> m JWT
loginUser username pwd = getUserByUsername username >>= \u -> do
  checkPassword pwd (uPassword u) >> createJwt (mkAuthUser u)

refreshJwtToken :: (Authentication m) => AuthUser -> m JWT
refreshJwtToken = createJwt

signupUser :: (Authentication m, UserRepository m) => NewUserData -> m (User, JWT)
signupUser userData = createUser userData >>= \u -> do
  createJwt (mkAuthUser u) <&> (u, )
