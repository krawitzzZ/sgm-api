module Api.Auth
  ( authServer
  , AuthApi
  ) where

import           Domain.Api                               ( ApiVersion(..) )
import           Domain.App                               ( AppM )
import           RIO                                      ( return )
import           Servant                                  ( type (:<|>)((:<|>))
                                                          , type (:>)
                                                          , JSON
                                                          , NoContent(..)
                                                          , Post
                                                          , ServerT
                                                          )


type Login = "login" :> Post '[JSON] NoContent
type Signup = "signup" :> Post '[JSON] NoContent

-- brittany-disable-next-binding
type AuthApi = Login :<|> Signup

authServer :: ApiVersion -> ServerT AuthApi AppM
authServer V1 = authV1Server

authV1Server :: ServerT AuthApi AppM
authV1Server = login :<|> signup

login :: AppM NoContent
login = return NoContent

signup :: AppM NoContent
signup = return NoContent
