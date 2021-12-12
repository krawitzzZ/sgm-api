module Infra.UserRepository
  ( get
  , findOne
  , createUser
  , upsertOne
  , deleteOne
  ) where

import           Control.Exception.Safe                   ( MonadThrow
                                                          , throwM
                                                          )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.User                              ( Id
                                                          , User(..)
                                                          , UserData(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , (==)
                                                          , Eq
                                                          , Exception
                                                          , Maybe(..)
                                                          , Monad
                                                          , Show
                                                          , fromMaybe
                                                          , return
                                                          )
import           RIO.List                                 ( find )


get :: Monad m => m [User]
get = return users

findOne :: (MonadThrow m) => Id -> m User
findOne id = case maybeUser of
  Just user -> return user
  Nothing   -> throwM . NotFound $ "User with id " <> id <> " not found"
  where maybeUser = find (\User { userId } -> userId == id) users

createUser :: MonadThrow m => UserData -> m User
createUser (UserData _ _ (Just "test@test.test") _) = throwM DatabaseNotFound
createUser (UserData fname lname email pass) = return $ User "123" name surname mail password
 where
  name     = fromMaybe "name" fname
  surname  = fromMaybe "surname" lname
  mail     = fromMaybe "mail" email
  password = fromMaybe "*********" pass

upsertOne :: Monad m => Id -> UserData -> m User
upsertOne id (UserData fname lname email pass) = return $ User id name surname mail password
 where
  name     = fromMaybe "name" fname
  surname  = fromMaybe "surname" lname
  mail     = fromMaybe "mail@mail.com" email
  password = fromMaybe "*********" pass

deleteOne :: Monad m => Id -> m ()
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

data DatabaseException = DatabaseNotFound
  deriving (Show, Eq)
instance Exception DatabaseException
