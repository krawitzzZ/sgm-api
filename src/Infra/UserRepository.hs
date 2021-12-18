module Infra.UserRepository
  ( getAll
  , findOneById
  , findOneByName
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
import           Domain.User                              ( User(..)
                                                          , UserData(..)
                                                          )
import           Infra.Beam.Mapper                        ( userEntityToDomain )
import           Infra.Beam.Query.User                    ( allUsers
                                                          , createAndInsertUser
                                                          , deleteUser
                                                          , maybeUserById
                                                          , maybeUserByName
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

findOneByName
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => Text -> m User
findOneByName name =
  withContext (mkContext "findUsers") $ tryCatchDefault $ maybeUserByName name >>= \case
    Nothing   -> throwM . NotFound $ "User with name '" <> name <> "' not found"
    Just user -> return $ userEntityToDomain user

createOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m)
  => UserData
  -> m User
createOne user@UserData { userDataName = name } =
  withContext (mkContext "createOne") $ tryCatchDefault $ maybeUserByName name >>= \case
    Just _  -> throwM $ UserNameAlreadyExists $ "User with name '" <> name <> "' already exists"
    Nothing -> createAndInsertUser user <&> userEntityToDomain

saveOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => User -> m User
saveOne user = withContext (mkContext "saveOne") $ tryCatchDefault $ updateUser user >> return user

deleteOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => UUID -> m ()
deleteOne = withContext (mkContext "deleteOne") . tryCatchDefault . deleteUser


mkContext :: LogContext -> LogContext
mkContext = ("Infra.UserRepository->" <>)
