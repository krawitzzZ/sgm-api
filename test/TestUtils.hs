module TestUtils
  ( mkUUID
  , uuid1
  , uuid2
  , uuid3
  ) where

import           Data.UUID                                          ( UUID
                                                                    , fromString
                                                                    , nil
                                                                    )
import           RIO                                                ( ($)
                                                                    , Text
                                                                    , fromMaybe
                                                                    )
import           RIO.Text                                           ( unpack )


mkUUID :: Text -> UUID
mkUUID t = fromMaybe nil $ fromString (unpack t)

uuid1 :: UUID
uuid1 = mkUUID "8526384c-451f-4307-88f8-0beed6bfe2e1"

uuid2 :: UUID
uuid2 = mkUUID "7f6ad9c5-6aa1-4c20-a64c-7d07fd99a969"

uuid3 :: UUID
uuid3 = mkUUID "3d6c4ed6-6b70-4f09-8503-090b1336777a"
