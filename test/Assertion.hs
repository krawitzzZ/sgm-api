module Assertion
  ( shouldFailWithStatus
  , shouldSatisfy'
  ) where

import           Network.HTTP.Types                                 ( Status )
import           RIO                                                ( ($)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Bool
                                                                    , Either(..)
                                                                    , IO
                                                                    , Show
                                                                    , show
                                                                    , unless
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

shouldSatisfy' :: HasCallStack => IO a -> (a -> Bool) -> Expectation
shouldSatisfy' action f = do
  thing <- action
  unless (f thing) $ expectationFailure "Returned value did not satisfy the provided criteria"
