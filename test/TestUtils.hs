module TestUtils
  ( mkUUID
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
