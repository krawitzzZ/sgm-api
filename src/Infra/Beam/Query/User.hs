module Infra.Beam.Query.User
  ( allUsers
  , maybeUserById
  , maybeUserByUsername
  , createAndInsertUser
  , updateUserInfo
  , deleteUser
  ) where

import           Control.Monad.Reader.Has                 ( Has )
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
import           Database.Beam.Postgres                   ( Connection )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto(..) )
import           Domain.Auth.Password                     ( hashPassword )
import           Domain.User                              ( User(..) )
import           Domain.User.UserData                     ( NewUserData(..) )
import           Infra.Beam.Query                         ( pgCrypto
                                                          , runBeam
                                                          , usersTable
                                                          )
import           Infra.Beam.Schema.Latest                 ( PrimaryKey(..)
                                                          , UserEntity
                                                          , UserEntityT(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , Maybe
                                                          , MonadIO
                                                          , Text
                                                          , return
                                                          )
import           RIO.Vector                               ( fromList )


allUsers :: (Has Connection c, MonadIO m) => c -> m [UserEntity]
allUsers c = runBeam c (runSelectReturningList $ select $ all_ usersTable)

maybeUserById :: (Has Connection c, MonadIO m) => c -> UUID -> m (Maybe UserEntity)
maybeUserById c uId = runBeam c $ runSelectReturningOne $ lookup_ usersTable (UserEntityId uId)

maybeUserByUsername :: (Has Connection c, MonadIO m) => c -> Text -> m (Maybe UserEntity)
maybeUserByUsername c username = runBeam c $ runSelectReturningOne $ select $ filter_
  (\UserEntity {..} -> ueUsername ==. val_ username)
  (all_ usersTable)

createAndInsertUser :: (Has Connection c, MonadIO m) => c -> NewUserData -> m UserEntity
createAndInsertUser c NewUserData {..} = runBeam c $ do
  let PgCrypto {..} = pgCrypto
  pwd          <- hashPassword nudPassword
  [userEntity] <- runInsertReturningList $ insert usersTable $ insertExpressions
    [ UserEntity { ueId            = pgCryptoGenRandomUUID
                 , ueCreatedAt     = currentTimestamp_
                 , ueLastUpdatedAt = currentTimestamp_
                 , ueUsername      = val_ nudUsername
                 , uePassword      = val_ pwd
                 , ueRoles         = val_ (fromList nudRoles)
                 , ueFirstName     = val_ nudFirstName
                 , ueLastName      = val_ nudLastName
                 }
    ]
  return userEntity

updateUserInfo :: (Has Connection c, MonadIO m) => c -> User -> m ()
updateUserInfo c User {..} = runBeam c $ runUpdate $ update
  usersTable
  (\UserEntity {..} ->
    (ueLastUpdatedAt <-. currentTimestamp_)
      <> (ueFirstName <-. val_ uFirstName)
      <> (ueLastName <-. val_ uLastName)
  )
  (\UserEntity {..} -> ueId ==. val_ uId)

deleteUser :: (Has Connection c, MonadIO m) => c -> UUID -> m ()
deleteUser c uId =
  runBeam c $ runDelete $ delete usersTable (\UserEntity {..} -> ueId ==. val_ uId)
