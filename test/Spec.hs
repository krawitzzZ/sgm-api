module Main
  ( main
  ) where

import           RIO                                      ( ($)
                                                          , Bool(..)
                                                          , IO
                                                          )
import           Test.Hspec
-- import           Test.Hspec.Wai
-- import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Dummy test" $ do
  it "says I will write more tests" $ do
    True `shouldBe` True
-- spec :: Spec
-- spec = with (return app) $ do
--     describe "GET /users" $ do
--         it "responds with 200" $ do
--             get "/users" `shouldRespondWith` 200
--         it "responds with [User]" $ do
--             let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
--             get "/users" `shouldRespondWith` users
