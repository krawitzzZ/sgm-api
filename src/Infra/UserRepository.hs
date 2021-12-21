module Infra.UserRepository
  ( getAll
  , findOneById
  , findOneByUsername
  , createOne
  , saveOne
  , deleteOne
  ) where

import           Control.Exception.Safe                   ( MonadCatch
                                                          , throwM
                                                          )
import           Control.Monad.Reader.Has                 ( Has )
import           Data.String.Conversions                  ( cs )
import           Data.UUID                                ( UUID )
import           Database.Beam.Postgres                   ( Connection )
import           Domain.Class                             ( MonadLogger(..) )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.Logger                            ( LogContext )
import           Domain.User                              ( NewUserData(..)
                                                          , User(..)
                                                          )
import           Infra.Beam.Mapper                        ( userEntityToDomain )
import           Infra.Beam.Query.User                    ( allUsers
                                                          , createAndInsertUser
                                                          , deleteUser
                                                          , maybeUserById
                                                          , maybeUserByUsername
                                                          , updateUser
                                                          )
import           Infra.Exception                          ( tryCatchDefault )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<&>)
                                                          , (<>)
                                                          , (>>)
                                                          , (>>=)
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , MonadReader
                                                          , Text
                                                          , map
                                                          , return
                                                          , show
                                                          )


getAll :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => m [User]
getAll = withContext (mkContext "getAll") $ tryCatchDefault $ allUsers <&> map userEntityToDomain

findOneById
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => UUID -> m User
findOneById id = withContext (mkContext "findUsers") $ tryCatchDefault $ maybeUserById id >>= \case
  Just user -> return $ userEntityToDomain user
  Nothing   -> throwM . NotFound $ "User with id '" <> cs (show id) <> "' not found"

findOneByUsername
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => Text -> m User
findOneByUsername username =
  withContext (mkContext "findUsers") $ tryCatchDefault $ maybeUserByUsername username >>= \case
    Nothing   -> throwM . NotFound $ "User with username '" <> username <> "' not found"
    Just user -> return $ userEntityToDomain user

createOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m)
  => NewUserData
  -> m User
createOne user@NewUserData { nudUsername } =
  withContext (mkContext "createOne") $ tryCatchDefault $ maybeUserByUsername nudUsername >>= \case
    Just _ ->
      throwM $ UserNameAlreadyExists $ "User with username '" <> nudUsername <> "' already exists"
    Nothing -> createAndInsertUser user <&> userEntityToDomain

saveOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => User -> m User
saveOne user = withContext (mkContext "saveOne") $ tryCatchDefault $ updateUser user >> return user

deleteOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => UUID -> m ()
deleteOne = withContext (mkContext "deleteOne") . tryCatchDefault . deleteUser


mkContext :: LogContext -> LogContext
mkContext = ("Infra.UserRepository->" <>)
