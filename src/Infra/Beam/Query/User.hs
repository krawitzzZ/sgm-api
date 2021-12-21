module Infra.Beam.Query.User
  ( allUsers
  , maybeUserById
  , maybeUserByUsername
  , createAndInsertUser
  , updateUser
  , deleteUser
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Data.String.Conversions                  ( cs )
import           Data.UUID                                ( UUID )
import           Database.Beam                            ( (<-.)
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
import           Database.Beam.Backend.SQL.BeamExtensions ( runInsertReturningList )
import           Database.Beam.Postgres                   ( Connection
                                                          , getPgExtension
                                                          )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto(..) )
import           Domain.Password                          ( hashPassword )
import           Domain.User                              ( NewUserData(..)
                                                          , User(..)
                                                          )
import           Infra.Beam.Query                         ( runBeam
                                                          , usersTable
                                                          )
import           Infra.Beam.Schema.Latest                 ( PrimaryKey(..)
                                                          , SgmDatabase(..)
                                                          , UserEntity
                                                          , UserEntityT(..)
                                                          , sgmDb
                                                          )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , Maybe
                                                          , MonadIO
                                                          , MonadReader
                                                          , Text
                                                          , return
                                                          )


allUsers :: (Has Connection e, MonadReader e m, MonadIO m) => m [UserEntity]
allUsers = runBeam (runSelectReturningList $ select $ all_ usersTable)

maybeUserById :: (Has Connection e, MonadReader e m, MonadIO m) => UUID -> m (Maybe UserEntity)
maybeUserById id = runBeam $ runSelectReturningOne $ lookup_ usersTable (UserEntityId id)

maybeUserByUsername
  :: (Has Connection e, MonadReader e m, MonadIO m) => Text -> m (Maybe UserEntity)
maybeUserByUsername username = runBeam $ runSelectReturningOne $ select $ filter_
  (\UserEntity { ueUsername } -> ueUsername ==. val_ (cs username))
  (all_ usersTable)

createAndInsertUser
  :: (Has Connection e, MonadReader e m, MonadIO m) => NewUserData -> m UserEntity
createAndInsertUser NewUserData { nudUsername, nudPassword, nudFirstName, nudLastName } =
  runBeam $ do
    let PgCrypto { pgCryptoGenRandomUUID = uuid } = getPgExtension (dbCryptoExtension sgmDb)
    pwd          <- hashPassword nudPassword
    [userEntity] <- runInsertReturningList $ insert usersTable $ insertExpressions
      [ UserEntity { ueId            = uuid
                   , ueCreatedAt     = currentTimestamp_
                   , ueLastUpdatedAt = currentTimestamp_
                   , ueUsername      = val_ nudUsername
                   , uePassword      = val_ pwd
                   , ueFirstName     = val_ nudFirstName
                   , ueLastName      = val_ nudLastName
                   }
      ]
    return userEntity

updateUser :: (Has Connection e, MonadReader e m, MonadIO m) => User -> m ()
updateUser user = runBeam $ runUpdate $ update
  usersTable
  (\UserEntity { ueUsername, ueLastUpdatedAt, ueFirstName, ueLastName } ->
    (ueUsername <-. val_ (uUsername user))
      <> (ueLastUpdatedAt <-. currentTimestamp_)
      <> (ueFirstName <-. val_ (uFirstName user))
      <> (ueLastName <-. val_ (uLastName user))
  )
  (\UserEntity { ueId } -> ueId ==. val_ (uId user))

deleteUser :: (Has Connection e, MonadReader e m, MonadIO m) => UUID -> m ()
deleteUser id = runBeam $ runDelete $ delete usersTable (\UserEntity { ueId } -> ueId ==. val_ id)
