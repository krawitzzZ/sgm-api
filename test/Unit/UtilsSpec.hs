module Unit.UtilsSpec
  ( spec
  ) where

import           RIO                                                ( ($)
                                                                    , Bool(..)
                                                                    , Int
                                                                    , Maybe(..)
                                                                    , Text
                                                                    )
import           Test.Hspec                                         ( Spec
                                                                    , describe
                                                                    , it
                                                                    , parallel
                                                                    , shouldBe
                                                                    )
import           Test.Hspec.QuickCheck                              ( prop )
import           Utils                                              ( anyElem
                                                                    , toMaybe
                                                                    )


spec :: Spec
spec = parallel $ do
  describe "toMaybe" $ do
    prop "should return Just" $ \(x :: Int) -> toMaybe True x `shouldBe` Just x
    prop "should return Nothing" $ \(x :: Int) -> toMaybe False x `shouldBe` Nothing

  describe "anyElem" $ do
    it "should return True" $ do
      anyElem [True] [True, False] `shouldBe` True
      anyElem [1 :: Int] [1, 2, 3] `shouldBe` True
      anyElem ["hi" :: Text] ["hi", "there"] `shouldBe` True
    it "should return False" $ do
      anyElem [True] [False] `shouldBe` False
      anyElem [1 :: Int] [2, 3] `shouldBe` False
      anyElem ["hi" :: Text] ["hey", "there"] `shouldBe` False
