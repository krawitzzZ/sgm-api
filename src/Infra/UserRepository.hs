module Infra.UserRepository
  ( get
  , findOne
  , saveOne
  , upsertOne
  , deleteOne
  ) where

import           Control.Exception.Safe                   ( throwM )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.User                              ( Id
                                                          , User(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , (==)
                                                          , IO
                                                          , Maybe(..)
                                                          , return
                                                          )
import           RIO.List                                 ( find )


get :: IO [User]
get = return users

findOne :: Id -> IO User
findOne id = case maybeUser of
  Just user -> return user
  Nothing   -> throwM . NotFound $ "User with id " <> id <> " not found"
  where maybeUser = find (\User { userId } -> userId == id) users

saveOne :: User -> IO ()
saveOne _ = return ()

upsertOne :: User -> IO ()
upsertOne _ = return ()

deleteOne :: Id -> IO ()
deleteOne _ = return ()



users :: [User]
users =
  [ User { userId        = "1"
         , userFirstName = "John"
         , userLastName  = "Doe"
         , userEmail     = "john@doe.com"
         , userPassword  = "*********"
         }
  , User { userId        = "2"
         , userFirstName = "Some"
         , userLastName  = "One"
         , userEmail     = "some@one.com"
         , userPassword  = "*********"
         }
  ]
