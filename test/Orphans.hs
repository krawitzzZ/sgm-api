{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans
  () where

import           RIO                                                ( Proxy(..) )
import           Servant                                            ( type (:>) )
import           Servant.Client                                     ( Client
                                                                    , HasClient(..)
                                                                    )
import           Servant.Exception                                  ( Throws )


instance (HasClient m api) => HasClient m (Throws e :> api) where
  type Client m (Throws e :> api) = Client m api
  clientWithRoute m _ req = clientWithRoute m (Proxy :: Proxy api) req
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt cl
