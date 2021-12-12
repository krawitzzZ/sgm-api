module Api.AuthApi
  ( authServer
  , AuthApi
  ) where

import           Domain.Api                               ( ApiVersion(..) )
import           RIO                                      ( Monad
                                                          , return
                                                          )
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

authServer :: Monad m => ApiVersion -> ServerT AuthApi m
authServer V1 = authV1Server

authV1Server :: Monad m => ServerT AuthApi m
authV1Server = login :<|> signup

login :: Monad m => m NoContent
login = return NoContent

signup :: Monad m => m NoContent
signup = return NoContent
