module Api.Exception
  ( ApiException(..)
  , tryCatch
  , tryCatches
  , tryCatchDefault
  , throw400
  , throw401
  , throw403
  , throw500
  ) where

import           Control.Exception.Safe                             ( Handler(..)
                                                                    , MonadCatch
                                                                    , MonadThrow
                                                                    , SomeException(..)
                                                                    , throwM
                                                                    )
import           Data.Aeson                                         ( (.=)
                                                                    , ToJSON(..)
                                                                    , object
                                                                    )
import           Domain.App.Class                                   ( MonadLogger(..) )
import           Domain.Exception                                   ( DomainException(..) )
import qualified Network.HTTP.Types                                as H
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , Exception(..)
                                                                    , Show(..)
                                                                    , Text
                                                                    )
import           Servant                                            ( MimeRender(..)
                                                                    , PlainText
                                                                    )
import           Servant.Exception.Server                           ( ToServantErr(..) )
import           Utils.Exception                                    ( mkTryCatch
                                                                    , mkTryCatchDefault
                                                                    , mkTryCatches
                                                                    )


data ApiException =
  BadRequest400 Text |
  Unauthorized401 |
  Forbidden403 |
  NotFound404 |
  Conflict409 Text |
  InternalError500
  deriving Show

instance Exception ApiException

instance ToJSON ApiException where
  toJSON e = object ["message" .= message e, "status" .= H.statusCode (status e)]

instance MimeRender PlainText ApiException where
  mimeRender ct = mimeRender ct . show

instance ToServantErr ApiException where
  status BadRequest400{}    = H.badRequest400
  status Unauthorized401{}  = H.unauthorized401
  status Forbidden403{}     = H.forbidden403
  status NotFound404{}      = H.notFound404
  status Conflict409{}      = H.conflict409
  status InternalError500{} = H.internalServerError500

  message (BadRequest400 msg) = msg
  message Unauthorized401     = "Unauthorized"
  message Forbidden403        = "Access forbidden"
  message NotFound404         = "Not found"
  message (Conflict409 msg)   = msg
  message InternalError500    = "Internal server error"

tryCatch :: (MonadLogger m, MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
tryCatch = mkTryCatch (Handler handleSomeException)

tryCatches :: (MonadLogger m, MonadCatch m) => m a -> [Handler m a] -> m a
tryCatches = mkTryCatches (Handler handleSomeException)

tryCatchDefault :: (MonadLogger m, MonadCatch m) => m a -> m a
tryCatchDefault = mkTryCatchDefault defaultHandlers

throw400 :: (MonadThrow m) => Text -> m a
throw400 msg = throwM $ BadRequest400 msg

throw401 :: (MonadThrow m) => m a
throw401 = throwM Unauthorized401

throw403 :: (MonadThrow m) => m a
throw403 = throwM Forbidden403

throw500 :: (MonadLogger m, MonadThrow m, Show err) => err -> m b
throw500 e = do
  withError e $ logWarn "Unexpected exception occurred"
  throwM InternalError500


handleDomainException :: (MonadThrow m, MonadLogger m) => (DomainException -> m a)
handleDomainException InvalidPassword{}             = throwM Unauthorized401
handleDomainException AccessPolicyViolation{}       = throwM Forbidden403
handleDomainException NotFound{}                    = throwM NotFound404
handleDomainException (  UserNameAlreadyExists msg) = throwM $ Conflict409 msg
handleDomainException e@(CreateJwtException    _  ) = do
  withError e $ logWarn "Failed to create JWT"
  throwM InternalError500
handleDomainException e@(InternalError _) = throw500 e

handleSomeException :: (MonadLogger m, MonadThrow m) => SomeException -> m a
handleSomeException = throw500

defaultHandlers :: (MonadLogger m, MonadCatch m) => [Handler m a]
defaultHandlers = [Handler handleDomainException, Handler handleSomeException]
