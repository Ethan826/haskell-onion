{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans
import Core.BankAccount (
  BankAccount (..),
 )
import Core.Id (Id (Id))
import Core.User (UserId (HumanId, OrganizationId))
import Data.ByteString.UTF8 (fromString)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Providers.InMemoryBankAccountRepository (
  InMemoryBankAccountAction,
  InMemoryBankAccountRepository (runInMemoryBankAccountRepository),
 )
import Providers.PostgresUserRepositoryV1 (
  PostgresUserRepositoryV1 (runPostgresUserRepositoryV1),
  PostgresUserRepositoryV1Env (
    PostgresUserRepositoryV1Env,
    postgresConnection
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

getPostgresEnv :: IO PostgresUserRepositoryV1Env
getPostgresEnv = do
  connString <- getEnv "POSTGRES_CONNECTION_STRING"
  conn <- connectPostgreSQL $ fromString connString
  pure $
    PostgresUserRepositoryV1Env{postgresConnection = conn}

main :: IO ()
main = do
  loadFile defaultConfig

  env <- getPostgresEnv
  let getUserByIdAction = runPostgresUserRepositoryV1 $ getUserById userId
  user <- runReaderT getUserByIdAction env
  print user

  let getAllHumansInOrganizationAction = runPostgresUserRepositoryV1 $ getAllHumansInOrganization orgId
  humans <- runReaderT getAllHumansInOrganizationAction env
  print humans
 where
  userId = OrganizationId $ Id 1
  orgId = Id 3

-- void $ runStateT modifyAndPrint []