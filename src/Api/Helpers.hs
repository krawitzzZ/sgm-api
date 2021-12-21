module Api.Helpers
  ( identityGuard
  ) where

import           Api.Exception                            ( throw403 )
import           Control.Exception.Safe                   ( MonadThrow )
import           Data.UUID                                ( UUID )
import           Domain.Auth                              ( AuthenticatedUser(..) )
import           RIO                                      ( (==)
                                                          , unless
                                                          )


identityGuard :: (MonadThrow m) => UUID -> AuthenticatedUser -> m ()
identityGuard id u = unless (auId u == id) throw403
