module Infra.Beam.Query.User
  ( allUsers
  , maybeUserById
  , maybeUserByUsername
  , createAndInsertUser
  , updateUserInfo
  , deleteUser
  ) where

import           Control.Monad.Reader.Has                           ( Has )
import           Database.Beam                                      ( (<-.)
                                                                    , (==.)
                                                                    , all_
                                                                    , currentTimestamp_
                                                                    , delete
                                                                    , filter_
                                                                    , insert
                                                                    , insertExpressions
                                                                    , lookup_
                                                                    , runDelete
                                                                    , runSelectReturningList
                                                                    , runSelectReturningOne
                                                                    , runUpdate
                                                                    , select
                                                                    , update
                                                                    , val_
                                                                    )
import           Database.Beam.Backend.SQL.BeamExtensions           ( runInsertReturningList )
import           Database.Beam.Postgres                             ( Connection )
import           Database.Beam.Postgres.PgCrypto                    ( PgCrypto(..) )
import           Domain.App.Config                                  ( Config )
import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.Password                               ( hashPassword )
import           Domain.User                                        ( User(..) )
import           Domain.User.UserData                               ( NewUserData(..) )
import           Infra.Beam.Query                                   ( pgCrypto
                                                                    , runBeam
                                                                    , usersTable
                                                                    )
import           Infra.Beam.Schema.Latest                           ( PrimaryKey(..)
                                                                    , UserEntity
                                                                    , UserEntityT(..)
                                                                    )
import           RIO                                                ( ($)
                                                                    , (<>)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , Text
                                                                    , return
                                                                    )
import           RIO.Vector                                         ( fromList )


allUsers :: (Has Connection e, Has Config e, MonadIO m) => e -> m [UserEntity]
allUsers e = runBeam e (runSelectReturningList $ select $ all_ usersTable)

maybeUserById :: (Has Connection e, Has Config e, MonadIO m) => e -> UserId -> m (Maybe UserEntity)
maybeUserById e userId =
  runBeam e $ runSelectReturningOne $ lookup_ usersTable (UserEntityId userId)

maybeUserByUsername
  :: (Has Connection e, Has Config e, MonadIO m) => e -> Text -> m (Maybe UserEntity)
maybeUserByUsername e username = runBeam e $ runSelectReturningOne $ select $ filter_
  (\UserEntity {..} -> ueUsername ==. val_ username)
  (all_ usersTable)

createAndInsertUser
  :: (Has Connection e, Has Config e, MonadIO m) => e -> NewUserData -> m UserEntity
createAndInsertUser e NewUserData {..} = runBeam e $ do
  let PgCrypto {..} = pgCrypto
  pwd          <- hashPassword nudPassword
  [userEntity] <- runInsertReturningList $ insert usersTable $ insertExpressions
    [ UserEntity { ueId             = pgCryptoGenRandomUUID
                 , ueCreatedAt      = currentTimestamp_
                 , ueLastUpdatedAt  = currentTimestamp_
                 , ueUsername       = val_ nudUsername
                 , ueProfilePicture = val_ Nothing
                 , uePassword       = val_ pwd
                 , ueRoles          = val_ (fromList nudRoles)
                 , ueFirstName      = val_ nudFirstName
                 , ueLastName       = val_ nudLastName
                 }
    ]
  return userEntity

updateUserInfo :: (Has Connection e, Has Config e, MonadIO m) => e -> User -> m ()
updateUserInfo e User {..} = runBeam e $ runUpdate $ update
  usersTable
  (\UserEntity {..} ->
    (ueLastUpdatedAt <-. currentTimestamp_)
      <> (ueFirstName <-. val_ uFirstName)
      <> (ueLastName <-. val_ uLastName)
  )
  (\UserEntity {..} -> ueId ==. val_ uId)

deleteUser :: (Has Connection e, Has Config e, MonadIO m) => e -> UserId -> m ()
deleteUser e userId =
  runBeam e $ runDelete $ delete usersTable (\UserEntity {..} -> ueId ==. val_ userId)
