module Infra.Exception
  ( tryCatchBeam
  , tryCatchBeamDefault
  ) where

import           Control.Exception.Safe                             ( Handler(..)
                                                                    , MonadCatch
                                                                    , MonadThrow
                                                                    , throwM
                                                                    )
import           Data.String.Conversions                            ( cs )
import           Domain.Exception                                   ( DomainException(..) )
import           Infra.Beam.Exception                               ( BeamException )
import           RIO                                                ( (.)
                                                                    , Exception(..)
                                                                    , show
                                                                    )
import           Utils.Exception                                    ( mkTryCatch
                                                                    , mkTryCatchDefault
                                                                    )


tryCatchBeam :: (MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
tryCatchBeam = mkTryCatch (Handler handleBeamException)

tryCatchBeamDefault :: (MonadCatch m) => m a -> m a
tryCatchBeamDefault = mkTryCatchDefault [Handler handleBeamException]


handleBeamException :: (MonadThrow m) => BeamException -> m a
handleBeamException = throwM . InternalError . cs . show
