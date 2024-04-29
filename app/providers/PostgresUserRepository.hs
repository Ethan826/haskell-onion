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
import Core.Human (Human (..), HumanId)
import Core.Id (Id (Id, unId))
import Core.Organization (OrganizationId)
import Core.User (User (HumanUser), UserId (HumanId, OrganizationId))
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (listToMaybe)
import Data.Text (pack)
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

getConn :: PostgresUserRepository Connection
getConn = do
  connString <- asks postgresUserRepositoryEnvConnectionString
  -- I know we'd use a pool in real life
  liftIO $ connectPostgreSQL $ fromString connString

instance UserRepository PostgresUserRepository where
  -- getUserById :: UserId -> PostgresUserRepository (Maybe User)
  getUserById :: HumanId -> PostgresUserRepository (Maybe User)
  getUserById humanId = do
    conn <- getConn
    humans <- liftIO $ query conn queryString [unId humanId]
    pure $ HumanUser . humanFromDbHuman <$> listToMaybe humans
   where
    queryString = "SELECT * FROM humans WHERE id = (?)"

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
