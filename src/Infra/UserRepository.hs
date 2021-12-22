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
                                                          , Text
                                                          , map
                                                          , return
                                                          , show
                                                          )


getAll :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> m [User]
getAll c =
  withContext (mkContext "getAll") $ tryCatchDefault $ allUsers c <&> map userEntityToDomain

findOneById :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> UUID -> m User
findOneById c id =
  withContext (mkContext "findOneById") $ tryCatchDefault $ maybeUserById c id >>= \case
    Just user -> return $ userEntityToDomain user
    Nothing   -> throwM . NotFound $ "User with id '" <> cs (show id) <> "' not found"

findOneByUsername
  :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> Text -> m User
findOneByUsername c username =
  withContext (mkContext "findOneByUsername")
    $   tryCatchDefault
    $   maybeUserByUsername c username
    >>= \case
          Nothing   -> throwM . NotFound $ "User with username '" <> username <> "' not found"
          Just user -> return $ userEntityToDomain user

createOne
  :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> NewUserData -> m User
createOne c user@NewUserData {..} =
  withContext (mkContext "createOne")
    $   tryCatchDefault
    $   maybeUserByUsername c nudUsername
    >>= \case
          Just _ ->
            throwM
              $  UserNameAlreadyExists
              $  "User with username '"
              <> nudUsername
              <> "' already exists"
          Nothing -> createAndInsertUser c user <&> userEntityToDomain

saveOne :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> User -> m User
saveOne c user =
  withContext (mkContext "saveOne") $ tryCatchDefault $ updateUser c user >> return user

deleteOne :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> UUID -> m ()
deleteOne c = withContext (mkContext "deleteOne") . tryCatchDefault . deleteUser c


mkContext :: LogContext -> LogContext
mkContext = ("Infra.UserRepository->" <>)
