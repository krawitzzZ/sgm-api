module Api
  ( server
  , SGMApi
  ) where

import           Api.ApiVersion                           ( ApiVersion )
import           Api.AuthApi                              ( AuthApi
                                                          , authServer
                                                          )
import           Api.UserApi                              ( UserApi
                                                          , userServer
                                                          )
import           Control.Exception.Safe                   ( MonadCatch )
import           Domain.Class                             ( Authentication
                                                          , MonadLogger
                                                          , UserRepository
                                                          )
import           Servant                                  ( type (:<|>)((:<|>))
                                                          , type (:>)
                                                          , Capture
                                                          , ServerT
                                                          )


-- brittany-disable-next-binding
type SGMApi auths = "api" :> Capture "version" ApiVersion :>
  (
    "auth" :> AuthApi auths :<|>
    "users" :> UserApi auths
  )

server
  :: (UserRepository m, Authentication m, MonadLogger m, MonadCatch m) => ServerT (SGMApi auths) m
server apiVersion = authServer apiVersion :<|> userServer apiVersion
