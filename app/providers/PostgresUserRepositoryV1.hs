{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.PostgresUserRepositoryV1 (
  PostgresUserRepositoryV1,
  PostgresUserRepositoryV1Action,
  PostgresUserRepositoryV1Env (..),
  runPostgresUserRepositoryV1,
) where

import Text.RawString.QQ (r)

import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks)
import Core.Human (Human (..))
import Core.Id (Id (Id, unId))
import Core.Organization (Organization (..), OrganizationId)
import Core.User (User (HumanUser, OrganizationUser), UserId (HumanId, OrganizationId))
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Data.Vector (Vector, fromList)
import Database.PostgreSQL.Simple (
  Connection,
  FromRow,
  query,
 )
import GHC.Generics (Generic)
import Services.UserRepository (UserRepository (..))

newtype PostgresUserRepositoryV1Env = PostgresUserRepositoryV1Env
  { postgresConnection :: Connection
  }

type PostgresUserRepositoryV1Action a =
  ReaderT PostgresUserRepositoryV1Env IO a

newtype PostgresUserRepositoryV1 a = PostgresUserRepositoryV1
  { runPostgresUserRepositoryV1 :: PostgresUserRepositoryV1Action a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader PostgresUserRepositoryV1Env
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

instance UserRepository PostgresUserRepositoryV1 where
  getUserById :: UserId -> PostgresUserRepositoryV1 (Maybe User)
  getUserById (HumanId userId) = do
    conn <- asks postgresConnection
    humans <- liftIO $ query conn queryString [unId userId]
    pure $ HumanUser . humanFromDbHuman <$> listToMaybe humans
   where
    queryString = "SELECT * FROM humans WHERE id = (?)"
  getUserById (OrganizationId userId) = do
    conn <- asks postgresConnection
    dbResult <- liftIO $ query conn queryString [unId userId]
    pure $ (listToMaybe . (tupleToOrganization <$>)) dbResult
   where
    tupleToOrganization :: (Int, Text, Vector Int) -> User
    tupleToOrganization
      ( organizationId
        , organizationName
        , organizationHumanIds
        ) =
        OrganizationUser $
          Organization
            { organizationId = Id organizationId
            , organizationName
            , organizationHumanIds = Id <$> organizationHumanIds
            }
    queryString =
      [r|
        SELECT o.id AS organization_id
               , o.name AS organization_name
               , ARRAY_AGG(oh.human_id) AS human_ids
        FROM organizations o
        INNER JOIN organization_humans oh
        ON o.id = oh.organization_id
        WHERE o.id = (?)
        GROUP BY o.id
      |]

  getAllHumansInOrganization ::
    OrganizationId ->
    PostgresUserRepositoryV1 (Vector Human)
  getAllHumansInOrganization organizationId = do
    conn <- asks postgresConnection
    humans :: [DbHuman] <- liftIO $ query conn queryString [unId organizationId]
    pure $ fromList $ humanFromDbHuman <$> humans
   where
    queryString =
      [r|
        SELECT h.id
               , h.name
        FROM organizations o
        INNER JOIN organization_humans oh
                ON o.id = oh.organization_id
        INNER JOIN humans h
                ON h.id = oh.human_id
        WHERE o.id = (?);
      |]
