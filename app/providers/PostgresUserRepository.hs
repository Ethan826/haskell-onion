{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Providers.PostgresUserRepository where

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Core.User (User)
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

instance UserRepository PostgresUserRepository where
  createUser user = do
    connectionString <-
      asks postgresUserRepositoryEnvConnectionString

    let x = 3

    pure ()

-- ‘getUserById’ and ‘getAllHumansInOrganization’