module TestConstants
  ( uuid1
  , uuid2
  , uuid3
  , superadminClaims
  , adminClaims
  , moderatorClaims
  , participantClaims
  ) where

import           Data.UUID                                          ( UUID )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           TestUtils                                          ( mkUUID )


uuid1, uuid2, uuid3, uuid4 :: UUID
uuid1 = mkUUID "8526384c-451f-4307-88f8-0beed6bfe2e1"
uuid2 = mkUUID "7f6ad9c5-6aa1-4c20-a64c-7d07fd99a969"
uuid3 = mkUUID "365b0205-686c-4d45-a6d2-eab77a5ec3a1"
uuid4 = mkUUID "e2d12ea8-f4cf-408c-a896-797d5d5d7060"

superadminClaims, adminClaims, moderatorClaims, participantClaims :: UserClaims
superadminClaims = UserClaims (UserId uuid1) [Superadmin]
adminClaims = UserClaims (UserId uuid2) [Admin]
moderatorClaims = UserClaims (UserId uuid3) [Moderator]
participantClaims = UserClaims (UserId uuid4) [Participant]
