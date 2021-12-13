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
import           Data.UUID                                ( fromText
                                                          , nil
                                                          , toText
                                                          )
import           Data.UUID.V4                             ( nextRandom )
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
                                                          , MonadIO
                                                          , Show
                                                          , fromMaybe
                                                          , liftIO
                                                          , return
                                                          )
import           RIO.List                                 ( find )


get :: MonadIO m => m [User]
get = users

findOne :: (MonadIO m, MonadThrow m) => Id -> m User
findOne id = do
  users' <- users
  case maybeUser users' of
    Just user -> return user
    Nothing   -> throwM . NotFound $ "User with id " <> toText id <> " not found"
  where maybeUser usrs = find (\User { userId } -> userId == id) usrs

createUser :: (MonadIO m, MonadThrow m) => UserData -> m User
createUser (UserData _     _     (Just "test@test.test") _   ) = throwM DatabaseNotFound
createUser (UserData fname lname email                   pass) = do
  id <- liftIO nextRandom
  return $ User id name surname mail password
 where
  name     = fromMaybe "name" fname
  surname  = fromMaybe "surname" lname
  mail     = fromMaybe "mail" email
  password = fromMaybe "*********" pass

upsertOne :: MonadIO m => Id -> UserData -> m User
upsertOne id (UserData fname lname email pass) = return $ User id name surname mail password
 where
  name     = fromMaybe "name" fname
  surname  = fromMaybe "surname" lname
  mail     = fromMaybe "mail@mail.com" email
  password = fromMaybe "*********" pass

deleteOne :: Monad m => Id -> m ()
deleteOne _ = return ()




users :: MonadIO m => m [User]
users = do
  return
    [ User { userId        = getId "aeadc5e2-2591-4730-9307-3913e9b95863"
           , userFirstName = "John"
           , userLastName  = "Doe"
           , userName      = "JohnDoe"
           , userPassword  = "*********"
           }
    , User { userId        = getId "e11978d7-808b-4658-a62d-40073916928b"
           , userFirstName = "Some"
           , userLastName  = "One"
           , userName      = "SomeOne"
           , userPassword  = "*********"
           }
    ]
  where getId = fromMaybe nil . fromText

data DatabaseException = DatabaseNotFound
  deriving (Show, Eq)
instance Exception DatabaseException
