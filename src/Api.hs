module Api
  ( server
  , SGMApi
  ) where

import           Api.AuthApi                              ( AuthApi
                                                          , authServer
                                                          )
import           Api.UserApi                              ( UserApi
                                                          , userServer
                                                          )
import           Control.Exception.Safe                   ( MonadCatch )
import           Domain.Api                               ( ApiVersion )
import           Domain.Logger.Class                      ( MonadLogger )
import           Domain.User                              ( UserRepository )
import           Servant                                  ( type (:<|>)((:<|>))
                                                          , type (:>)
                                                          , Capture
                                                          , ServerT
                                                          )


-- brittany-disable-next-binding
type SGMApi = "api" :> Capture "version" ApiVersion :>
  (
    "users" :> UserApi :<|>
    "auth" :> AuthApi
  )

server :: (UserRepository m, MonadCatch m, MonadLogger m) => ServerT SGMApi m
server apiVersion = userServer apiVersion :<|> authServer apiVersion
