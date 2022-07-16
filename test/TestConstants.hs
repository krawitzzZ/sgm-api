module TestConstants
  ( uuid1
  , uuid2
  , superadminClaims
  , adminClaims
  , moderatorClaims
  , participantClaims
  ) where

import           Data.UUID                                          ( UUID
                                                                    , nil
                                                                    )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           TestUtils                                          ( mkUUID )


uuid1 :: UUID
uuid1 = mkUUID "8526384c-451f-4307-88f8-0beed6bfe2e1"

uuid2 :: UUID
uuid2 = mkUUID "7f6ad9c5-6aa1-4c20-a64c-7d07fd99a969"

superadminClaims :: UserClaims
superadminClaims = UserClaims (UserId nil) [Superadmin]

adminClaims :: UserClaims
adminClaims = UserClaims (UserId nil) [Admin]

moderatorClaims :: UserClaims
moderatorClaims = UserClaims (UserId nil) [Moderator]

participantClaims :: UserClaims
participantClaims = UserClaims (UserId nil) [Participant]
