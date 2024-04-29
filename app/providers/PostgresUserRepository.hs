{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Providers.PostgresUserRepository where

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Core.User (User)
import Data.ByteString.UTF8 (fromString)
import Database.PostgreSQL.Simple
import Services.UserRepository (UserRepository (..))

data PostgresUserRepositoryEnv = PostgresUserRepositoryEnv
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
    )

query = "INSERT INTO users ()"

instance UserRepository PostgresUserRepository where
  createUser :: User -> PostgresUserRepository ()
  createUser user = do
    connectionString <-
      asks postgresUserRepositoryEnvConnectionString

    let conn = connectPostgreSQL $ fromString connectionString

    pure ()

hello :: IO Int
hello = do
  conn <- connectPostgreSQL ""
  [Only i] <- query_ conn "select 2 + 2"
  return i