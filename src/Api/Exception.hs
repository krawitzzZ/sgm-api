module Api.Exception
  ( ApiException(..)
  , tryCatch
  , tryCatches
  , tryCatchDefault
  ) where

import           Control.Exception.Safe                   ( Handler(..)
                                                          , MonadCatch
                                                          , MonadThrow
                                                          , SomeException(..)
                                                          , throwM
                                                          )
import           Data.Aeson                               ( (.=)
                                                          , ToJSON(..)
                                                          , object
                                                          )
import           Domain.Class                             ( MonadLogger(..) )
import           Domain.Exception                         ( DomainException(..) )
import qualified Network.HTTP.Types                      as H
import           RIO                                      ( ($)
                                                          , (.)
                                                          , Eq
                                                          , Exception(..)
                                                          , Show(..)
                                                          , Text
                                                          )
import           Servant                                  ( MimeRender(..)
                                                          , PlainText
                                                          )
import           Servant.Exception.Server                 ( ToServantErr(..) )
import           Utils.Exception                          ( mkTryCatch
                                                          , mkTryCatchDefault
                                                          , mkTryCatches
                                                          )


data ApiException =
  InternalError500 |
  BadRequest400 Text |
  NotFound404 Text |
  Conflict409 Text
  deriving (Eq, Show)

instance Exception ApiException

instance ToJSON ApiException where
  toJSON e = object ["message" .= message e, "status" .= H.statusCode (status e)]

instance MimeRender PlainText ApiException where
  mimeRender ct = mimeRender ct . show

instance ToServantErr ApiException where
  status InternalError500{} = H.internalServerError500
  status BadRequest400{}    = H.badRequest400
  status NotFound404{}      = H.notFound404
  status Conflict409{}      = H.conflict409

  message InternalError500    = "Internal Server Error"
  message (BadRequest400 msg) = msg
  message (NotFound404   msg) = msg
  message (Conflict409   msg) = msg

tryCatch :: (MonadLogger m, MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
tryCatch = mkTryCatch (Handler handleSomeException)

tryCatches :: (MonadLogger m, MonadCatch m) => m a -> [Handler m a] -> m a
tryCatches = mkTryCatches (Handler handleSomeException)

tryCatchDefault :: (MonadLogger m, MonadCatch m) => m a -> m a
tryCatchDefault = mkTryCatchDefault defaultHandlers


handleDomainException :: (MonadThrow m, MonadLogger m) => (DomainException -> m a)
handleDomainException NotFound{}                  = throwM $ NotFound404 "Not found"
handleDomainException (UserNameAlreadyExists msg) = throwM $ Conflict409 msg
handleDomainException e                           = do
  withError e $ logWarn "Unhandled domain exception occurred"
  throwM InternalError500

handleSomeException :: (MonadLogger m, MonadThrow m) => SomeException -> m a
handleSomeException (SomeException e) = do
  withError e $ logWarn "Unexpected exception occurred"
  throwM InternalError500

defaultHandlers :: (MonadLogger m, MonadCatch m) => [Handler m a]
defaultHandlers = [Handler handleDomainException, Handler handleSomeException]
