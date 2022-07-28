module Infra.UserRepository
  ( getAll
  , findOneById
  , findOneByUsername
  , createOne
  , saveOne
  , deleteOne
  ) where

import           Control.Exception.Safe                             ( MonadCatch
                                                                    , throwM
                                                                    )
import           Control.Monad.Reader.Has                           ( Has )
import           Data.UUID                                          ( toText )
import           Database.Beam.Postgres                             ( Connection )
import           Domain.App.Config                                  ( Config )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.User                                        ( User(..) )
import           Domain.User.UserData                               ( NewUserData(..) )
import           Infra.Beam.Mapper                                  ( userEntityToDomain )
import           Infra.Beam.Query.User                              ( allUsers
                                                                    , createAndInsertUser
                                                                    , deleteUser
                                                                    , maybeUserById
                                                                    , maybeUserByUsername
                                                                    , updateUserInfo
                                                                    )
import           Infra.Exception                                    ( tryCatchBeamDefault )
import           RIO                                                ( ($)
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
                                                                    )


getAll :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> m [User]
getAll e = tryCatchBeamDefault $ allUsers e <&> map userEntityToDomain

findOneById :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> UserId -> m User
findOneById e userId = tryCatchBeamDefault $ maybeUserById e userId >>= \case
  Nothing   -> throwM . NotFound $ "User with id '" <> toText (unUserId userId) <> "' not found"
  Just user -> return $ userEntityToDomain user

findOneByUsername
  :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> Text -> m User
findOneByUsername e username = tryCatchBeamDefault $ maybeUserByUsername e username >>= \case
  Nothing   -> throwM . NotFound $ "User with username '" <> username <> "' not found"
  Just user -> return $ userEntityToDomain user

createOne
  :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> NewUserData -> m User
createOne e user@NewUserData {..} =
  tryCatchBeamDefault $ maybeUserByUsername e nudUsername >>= \case
    Just _ ->
      throwM . UserNameAlreadyExists $ "User with username '" <> nudUsername <> "' already exists"
    Nothing -> createAndInsertUser e user <&> userEntityToDomain

saveOne :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> User -> m User
saveOne e user = tryCatchBeamDefault $ updateUserInfo e user >> return user

deleteOne :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> UserId -> m ()
deleteOne e userId = tryCatchBeamDefault $ maybeUserById e userId >>= \case
  Nothing -> throwM . NotFound $ "User with id '" <> toText (unUserId userId) <> "' not found"
  Just _  -> deleteUser e userId
