module Api.Exception
  ( ApiException(..)
  ) where

import           Data.Aeson                               ( (.=)
                                                          , ToJSON(..)
                                                          , object
                                                          )
import qualified Network.HTTP.Types                      as H
import           RIO                                      ( (.)
                                                          , Eq
                                                          , Exception(..)
                                                          , Generic
                                                          , Show(..)
                                                          , Text
                                                          , Typeable
                                                          )
import           Servant                                  ( MimeRender(..)
                                                          , PlainText
                                                          )
import           Servant.Exception.Server                 ( ToServantErr(..) )


data ApiException =
  InternalError |
  BadRequest Text |
  NotFound Text
  deriving (Eq, Show, Generic, Typeable)
instance Exception ApiException
instance ToJSON ApiException where
  toJSON e = object ["message" .= message e, "status" .= H.statusCode (status e)]
instance MimeRender PlainText ApiException where
  mimeRender ct = mimeRender ct . show

instance ToServantErr ApiException where
  status BadRequest{}    = H.status400
  status NotFound{}      = H.status404
  status InternalError{} = H.status500

  message (BadRequest msg) = msg
  message (NotFound   msg) = msg
  message InternalError    = "Internal Server Error"
