module Api
  ( server
  , SGMApi
  ) where

import           Api.Auth                                 ( AuthApi
                                                          , authServer
                                                          )
import           Api.User                                 ( UserApi
                                                          , userServer
                                                          )
import           Domain.Api                               ( ApiVersion )
import           Domain.App                               ( AppM )
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

server :: ServerT SGMApi AppM
server apiVersion = userServer apiVersion :<|> authServer apiVersion
