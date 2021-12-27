module Api
  ( server
  , SGMApi
  ) where

import           Api.ApiVersion                                     ( ApiVersion )
import           Api.AuthApi                                        ( AuthApi
                                                                    , authServer
                                                                    )
import           Api.EventApi                                       ( EventApi
                                                                    , eventServer
                                                                    )
import           Api.UserApi                                        ( UserApi
                                                                    , userServer
                                                                    )
import           Control.Exception.Safe                             ( MonadCatch )
import           Domain.App.Class                                   ( Authentication
                                                                    , EventRepository
                                                                    , MonadLogger
                                                                    , UserRepository
                                                                    )
import           Servant                                            ( type (:<|>)((:<|>))
                                                                    , type (:>)
                                                                    , Capture
                                                                    , ServerT
                                                                    )


-- brittany-disable-next-binding
type SGMApi auths = "api" :> Capture "version" ApiVersion :>
  (
    "auth" :> AuthApi auths :<|>
    "users" :> UserApi auths :<|>
    "events" :> EventApi auths
  )

server
  :: (Authentication m, UserRepository m, EventRepository m, MonadLogger m, MonadCatch m)
  => ServerT (SGMApi auths) m
server apiVersion = authServer apiVersion :<|> userServer apiVersion :<|> eventServer apiVersion
