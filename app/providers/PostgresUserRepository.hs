{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.PostgresUserRepository (
  PostgresUserRepository,
  PostgresUserRepositoryAction,
  PostgresUserRepositoryEnv (..),
  runPostgresUserRepository,
) where

import Text.RawString.QQ (r)

import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks)
import Core.Human (Human (..))
import Core.Id (Id (Id, unId))
import Core.Organization (Organization (..), OrganizationId)
import Core.User (User (HumanUser, OrganizationUser), UserId (HumanId, OrganizationId))
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple (
  Connection,
  FromRow,
  connectPostgreSQL,
  query,
 )
import GHC.Generics (Generic)
import Services.UserRepository (UserRepository (..))

newtype PostgresUserRepositoryEnv = PostgresUserRepositoryEnv
  { postgresUserRepositoryEnvConnectionString :: String
  }

type PostgresUserRepositoryAction a =
  ReaderT PostgresUserRepositoryEnv IO a

newtype PostgresUserRepository a = PostgresUserRepository
  { runPostgresUserRepository :: PostgresUserRepositoryAction a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader PostgresUserRepositoryEnv
    , MonadIO
    )

data DbHuman = DbHuman {dbHumanId :: Int, dbHumanName :: String}
  deriving (Generic, Show, Eq)

instance FromRow DbHuman

humanFromDbHuman :: DbHuman -> Human
humanFromDbHuman DbHuman{..} =
  Human
    { humanId = Id dbHumanId
    , humanName = pack dbHumanName
    }

data DbOrganization = DbOrganization {dbOrganizationId :: Int, dbOrganizationName :: String}
  deriving (Generic, Show, Eq)

instance FromRow DbOrganization

getConn :: PostgresUserRepository Connection
getConn = do
  connString <- asks postgresUserRepositoryEnvConnectionString
  -- I know we'd use a pool in real life
  liftIO $ connectPostgreSQL $ fromString connString

type NameIdTuples = [(Text, Int)]

organizationUserFromNameIdTuples :: NameIdTuples -> OrganizationId -> Maybe User
organizationUserFromNameIdTuples tuples userId =
  ( \orgName ->
      OrganizationUser $
        Organization
          { organizationName = orgName
          , organizationId = userId
          , organizationHumanIds = Id . snd <$> tuples
          }
  )
    <$> maybeOrgName
 where
  maybeOrgName = fst <$> listToMaybe tuples

instance UserRepository PostgresUserRepository where
  getUserById :: UserId -> PostgresUserRepository (Maybe User)
  getUserById (HumanId userId) = do
    conn <- getConn
    humans <- liftIO $ query conn queryString [unId userId]
    pure $ HumanUser . humanFromDbHuman <$> listToMaybe humans
   where
    queryString = "SELECT * FROM humans WHERE id = (?)"
  getUserById (OrganizationId userId) = do
    conn <- getConn
    result <- liftIO $ query conn queryString [unId userId]
    pure $ organizationUserFromNameIdTuples result userId
   where
    queryString =
      [r|
        SELECT o.name
              , oh.human_id
        FROM organizations o
        INNER JOIN organization_humans oh
                ON oh.organization_id = o.id
        WHERE o.id = (?)
      |]

  getAllHumansInOrganization ::
    OrganizationId ->
    PostgresUserRepository [Human]
  getAllHumansInOrganization organizationId = do
    conn <- getConn
    humans :: [DbHuman] <- liftIO $ query conn queryString [unId organizationId]
    pure $ humanFromDbHuman <$> humans
   where
    queryString =
      [r|
        SELECT h.*
        FROM organizations o
        INNER JOIN organization_humans oh
            ON o.id = oh.organization_id
        INNER JOIN humans h
            ON h.id = oh.human_id
        WHERE o.id = (?);
      |]
