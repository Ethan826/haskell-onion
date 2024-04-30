{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans
import Core.BankAccount (
  BankAccount (..),
 )
import Core.Id (Id (Id))
import Core.User (UserId (HumanId, OrganizationId))
import Providers.InMemoryBankAccountRepository (
  InMemoryBankAccountAction,
  InMemoryBankAccountRepository (runInMemoryBankAccountRepository),
 )
import Providers.PostgresUserRepository (
  PostgresUserRepository (runPostgresUserRepository),
  PostgresUserRepositoryEnv (
    PostgresUserRepositoryEnv,
    postgresUserRepositoryEnvConnectionString
  ),
 )
import Services.BankAccountRepository (
  BankAccountRepository (..),
 )
import Services.UserRepository (UserRepository (getAllHumansInOrganization, getUserById))
import System.Environment (getEnv)

someData :: BankAccount
someData =
  BankAccount
    { accountNumber = Id 123
    , balanceCents = 10000
    , bankAccountHolderId = HumanId $ Id 987
    }

modifyAndPrint :: InMemoryBankAccountAction ()
modifyAndPrint = runInMemoryBankAccountRepository $ do
  createAccount someData
  account <- getAccountById (Id 123)
  liftIO $ print account

getPostgresEnv :: IO PostgresUserRepositoryEnv
getPostgresEnv = do
  postgresUserRepositoryEnvConnectionString <- getEnv "POSTGRES_CONNECTION_STRING"
  pure $
    PostgresUserRepositoryEnv
      { postgresUserRepositoryEnvConnectionString
      }

main :: IO ()
main = do
  loadFile defaultConfig

  env <- getPostgresEnv
  let getUserByIdAction = runPostgresUserRepository $ getUserById userId
  user <- runReaderT getUserByIdAction env
  print user

  let getAllHumansInOrganizationAction = runPostgresUserRepository $ getAllHumansInOrganization orgId
  humans <- runReaderT getAllHumansInOrganizationAction env
  print humans
 where
  userId = OrganizationId $ Id 1
  orgId = Id 3

-- void $ runStateT modifyAndPrint []