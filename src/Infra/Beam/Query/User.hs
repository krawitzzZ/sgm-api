module Infra.Beam.Query.User
  ( allUsers
  , maybeUserById
  , maybeUserByName
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
import           Domain.User                              ( User(..)
                                                          , UserData(..)
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

maybeUserByName :: (Has Connection e, MonadReader e m, MonadIO m) => Text -> m (Maybe UserEntity)
maybeUserByName name = runBeam $ runSelectReturningOne $ select $ filter_
  (\u -> userEntityName u ==. val_ (cs name))
  (all_ usersTable)

createAndInsertUser :: (Has Connection e, MonadReader e m, MonadIO m) => UserData -> m UserEntity
createAndInsertUser (UserData name pass fname lname) = runBeam $ do
  let PgCrypto { pgCryptoGenRandomUUID = uuid } = getPgExtension (dbCryptoExtension sgmDb)
  pwd          <- hashPassword pass
  [userEntity] <- runInsertReturningList $ insert usersTable $ insertExpressions
    [ UserEntity uuid
                 currentTimestamp_
                 currentTimestamp_
                 (val_ name)
                 (val_ pwd)
                 (val_ fname)
                 (val_ lname)
    ]
  return userEntity

updateUser :: (Has Connection e, MonadReader e m, MonadIO m) => User -> m ()
updateUser user = runBeam $ runUpdate $ update
  usersTable
  (\u ->
    (userEntityName u <-. val_ (userName user))
      <> (userEntityLastUpdatedAt u <-. currentTimestamp_)
      <> (userEntityFirstName u <-. val_ (userFirstName user))
      <> (userEntityLastName u <-. val_ (userLastName user))
  )
  (\u -> userEntityId u ==. val_ (userId user))

deleteUser :: (Has Connection e, MonadReader e m, MonadIO m) => UUID -> m ()
deleteUser id = runBeam $ runDelete $ delete usersTable (\u -> userEntityId u ==. val_ id)
