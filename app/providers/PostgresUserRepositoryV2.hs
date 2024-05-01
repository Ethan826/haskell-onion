{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.PostgresUserRepositoryV2 (
  PostgresUserRepositoryV2,
  PostgresUserRepositoryV2Action,
  PostgresUserRepositoryV2Env (..),
  runPostgresUserRepositoryV2,
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

instance UserRepository PostgresUserRepositoryV2 where
  getUserById :: UserId -> PostgresUserRepositoryV2 (Maybe User)
  getUserById (HumanId userId) = do
    humans <- runQuery humanByIdQuery [unId userId]
    pure $ HumanUser . humanFromDbHuman <$> listToMaybe humans
  getUserById (OrganizationId userId) = do
    dbResult <- runQuery organizationByIdQuery [unId userId]
    pure $ (listToMaybe . (organizationFromDb <$>)) dbResult

  getAllHumansInOrganization ::
    OrganizationId ->
    PostgresUserRepositoryV2 (Vector Human)
  getAllHumansInOrganization organizationId = do
    humans <- runQuery humansInOrganizationQuery [unId organizationId]
    pure $ fromList $ humanFromDbHuman <$> humans

newtype PostgresUserRepositoryV2Env = PostgresUserRepositoryV2Env
  { postgresConnection :: Connection
  }

type PostgresUserRepositoryV2Action a =
  ReaderT PostgresUserRepositoryV2Env IO a

newtype PostgresUserRepositoryV2 a = PostgresUserRepositoryV2
  { runPostgresUserRepositoryV2 :: PostgresUserRepositoryV2Action a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader PostgresUserRepositoryV2Env
    , MonadIO
    )

------------------------------------------------------------
-- SQL Queries
------------------------------------------------------------

humanByIdQuery :: Query
humanByIdQuery =
  "SELECT id, name FROM users WHERE id = (?);"

organizationByIdQuery :: Query
organizationByIdQuery =
  [r| SELECT oh.organization_id
       , ou.name AS organization_name
       , ARRAY_AGG(oh.human_id) as human_ids
FROM users ou
INNER JOIN organization_humans oh
        ON oh.organization_id = ou.id
WHERE ou.id = (?) 
GROUP BY oh.organization_id, ou.name; |]

humansInOrganizationQuery :: Query
humansInOrganizationQuery =
  [r| SELECT hu.id, hu.name
FROM users ou
INNER JOIN organization_humans oh
        ON oh.organization_id = ou.id
INNER JOIN users hu
        ON hu.id = oh.human_id
WHERE ou.id = (?); |]

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
  PostgresUserRepositoryV2 [r]
runQuery q params = do
  conn <- asks postgresConnection
  liftIO $ query conn q params
