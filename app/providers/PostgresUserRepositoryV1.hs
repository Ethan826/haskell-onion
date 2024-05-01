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
  Query,
  ToRow,
  query,
 )
import GHC.Generics (Generic)
import Services.UserRepository (UserRepository (..))

------------------------------------------------------------
-- Main export
------------------------------------------------------------

instance UserRepository PostgresUserRepositoryV1 where
  getUserById :: UserId -> PostgresUserRepositoryV1 (Maybe User)
  getUserById (HumanId userId) = do
    humans <- runQuery userByIdQuery [unId userId]
    pure $ HumanUser . humanFromDbHuman <$> listToMaybe humans
  getUserById (OrganizationId userId) = do
    dbResult <- runQuery organizationByIdQuery [unId userId]
    pure $ (listToMaybe . (organizationFromDb <$>)) dbResult

  getAllHumansInOrganization ::
    OrganizationId ->
    PostgresUserRepositoryV1 (Vector Human)
  getAllHumansInOrganization organizationId = do
    humans <- runQuery humansInOrganizationQuery [unId organizationId]
    pure $ fromList $ humanFromDbHuman <$> humans

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

------------------------------------------------------------
-- SQL Queries
------------------------------------------------------------

userByIdQuery :: Query
userByIdQuery =
  "SELECT id, name FROM humans WHERE id = (?)"

organizationByIdQuery :: Query
organizationByIdQuery =
  [r|
SELECT o.id AS organization_id
        , o.name AS organization_name
        , ARRAY_AGG(oh.human_id) AS human_ids
FROM organizations o
INNER JOIN organization_humans oh ON o.id = oh.organization_id
WHERE o.id = (?)
GROUP BY o.id; |]

humansInOrganizationQuery :: Query
humansInOrganizationQuery =
  [r|
SELECT h.*
FROM organizations o
INNER JOIN organization_humans oh ON o.id = oh.organization_id
INNER JOIN humans h ON h.id = oh.human_id
WHERE o.id = (?); |]

------------------------------------------------------------
-- DB -> Domain conversions
------------------------------------------------------------

data DbHuman = DbHuman
  { dbHumanId :: Int
  , dbHumanName :: String
  }
  deriving (Generic, Show, Eq)

instance FromRow DbHuman

humanFromDbHuman :: DbHuman -> Human
humanFromDbHuman DbHuman{..} =
  Human
    { humanId = Id dbHumanId
    , humanName = pack dbHumanName
    }

data DbOrganization = DbOrganization
  { dbOrganizationId :: Int
  , dbOrganizationName :: String
  }
  deriving (Generic, Show, Eq)

instance FromRow DbOrganization

organizationFromDb :: (Int, Text, Vector Int) -> User
organizationFromDb (orgId, orgName, orgHumanIds) =
  OrganizationUser $
    Organization
      { organizationId = Id orgId
      , organizationName = orgName
      , organizationHumanIds = Id <$> orgHumanIds
      }

runQuery ::
  (ToRow q, FromRow r) =>
  Query ->
  q ->
  PostgresUserRepositoryV1 [r]
runQuery q params = do
  conn <- asks postgresConnection
  liftIO $ query conn q params
