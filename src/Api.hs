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
import           Control.Monad.Reader.Has                 ( Has )
import           Control.Monad.Time                       ( MonadTime )
import           Domain.Class                             ( MonadLogger
                                                          , UserRepository
                                                          )
import           RIO                                      ( MonadIO
                                                          , MonadReader
                                                          )
import           Servant                                  ( type (:<|>)((:<|>))
                                                          , type (:>)
                                                          , Capture
                                                          , ServerT
                                                          )
import           Servant.Auth.Server                      ( JWTSettings )


-- brittany-disable-next-binding
type SGMApi auths = "api" :> Capture "version" ApiVersion :>
  (
    "auth" :> AuthApi auths :<|>
    "users" :> UserApi auths
  )

server
  :: ( Has JWTSettings e
     , MonadReader e m
     , UserRepository m
     , MonadLogger m
     , MonadCatch m
     , MonadTime m
     , MonadIO m
     )
  => ServerT (SGMApi auths) m
server apiVersion = authServer apiVersion :<|> userServer apiVersion
