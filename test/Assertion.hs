module Assertion
  ( shouldFailWithStatus
  ) where

import           Network.HTTP.Types                                 ( Status )
import           RIO                                                ( ($)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Either(..)
                                                                    , IO
                                                                    , Show(show)
                                                                    )
import           Servant.Client                                     ( ClientError(..)
                                                                    , ResponseF(..)
                                                                    )
import           Test.Hspec                                         ( Expectation
                                                                    , HasCallStack
                                                                    , expectationFailure
                                                                    , shouldBe
                                                                    )


shouldFailWithStatus
  :: (HasCallStack, Show a) => IO (Either ClientError a) -> Status -> Expectation
shouldFailWithStatus getEitherRes code = getEitherRes >>= \case
  (Left (FailureResponse _ Response {..})) -> responseStatusCode `shouldBe` code
  (Left e) -> expectationFailure $ "Unexpected ClientError: " <> show e
  Right r -> expectationFailure $ "Unexpected successful response: " <> show r
