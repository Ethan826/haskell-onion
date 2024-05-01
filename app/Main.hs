{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader (ReaderT (runReaderT))
import Core.Id (Id (Id))
import Core.User (UserId (HumanId, OrganizationId))
import Data.ByteString.UTF8 (fromString)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Providers.PostgresUserRepositoryV1 (
  PostgresUserRepositoryV1 (runPostgresUserRepositoryV1),
  PostgresUserRepositoryV1Env (PostgresUserRepositoryV1Env, postgresConnectionV1),
  createUserRepositoryV1Actions,
 )
import Providers.PostgresUserRepositoryV2 (
  PostgresUserRepositoryV2 (runPostgresUserRepositoryV2),
  PostgresUserRepositoryV2Env (..),
 )
import Providers.SqliteBankAccountRepository (
  SqliteBankAccountRepository (runSqliteBankAccountRepository),
  SqliteBankAccountRepositoryEnv (
    SqliteBankAccountRepositoryEnv,
    sqliteFileLocation,
    userRepositoryActions
  ),
 )
import Services.BankAccountRepository (BankAccountRepository (getAccountById))
import Services.UserRepository (
  UserRepository (getAllHumansInOrganization, getUserById),
  UserRepositoryActions,
 )
import System.Environment (getEnv)

getPostgresEnvV1 :: IO PostgresUserRepositoryV1Env
getPostgresEnvV1 = do
  connString <- getEnv "POSTGRES_CONNECTION_STRING_V1"
  conn <- connectPostgreSQL $ fromString connString
  pure $
    PostgresUserRepositoryV1Env{postgresConnectionV1 = conn}

getPostgresEnvV2 :: IO PostgresUserRepositoryV2Env
getPostgresEnvV2 = do
  connString <- getEnv "POSTGRES_CONNECTION_STRING_V2"
  conn <- connectPostgreSQL $ fromString connString
  pure $
    PostgresUserRepositoryV2Env{postgresConnectionV2 = conn}

getSqliteBankAccountRepositoryEnv :: UserRepositoryActions -> SqliteBankAccountRepositoryEnv
getSqliteBankAccountRepositoryEnv userRepositoryActions =
  SqliteBankAccountRepositoryEnv
    { sqliteFileLocation = "./accounts.db"
    , userRepositoryActions
    }

main :: IO ()
main = do
  loadFile defaultConfig

  envV1 <- getPostgresEnvV1
  envV2 <- getPostgresEnvV2

  let accountsEnv = getSqliteBankAccountRepositoryEnv $ createUserRepositoryV1Actions envV1
  let getAccountByIdAction = runSqliteBankAccountRepository $ getAccountById $ Id 1
  runReaderT getAccountByIdAction accountsEnv >>= print

  putStrLn "Let's look up user 1 in V1"

  let getHumanByIdActionV1 = runPostgresUserRepositoryV1 $ getUserById userId
  runReaderT getHumanByIdActionV1 envV1 >>= print

  putStrLn "\nNow let's see user 1 in V2"

  let getHumanByIdActionV2 = runPostgresUserRepositoryV2 $ getUserById userId
  runReaderT getHumanByIdActionV2 envV2 >>= print

  putStrLn "\nNow we'll get the engineers in V1"

  let getOrganizationByIdActionV1 = runPostgresUserRepositoryV1 $ getUserById $ OrganizationId engineersIdV1
  runReaderT getOrganizationByIdActionV1 envV1 >>= print

  putStrLn "\nAnd the engineers in V2"

  let getOrganizationByIdActionV2 = runPostgresUserRepositoryV2 $ getUserById $ OrganizationId engineersIdV2
  runReaderT getOrganizationByIdActionV2 envV2 >>= print

  putStrLn "\nFinally, we'll get all users in the engineering org, V1"

  let engineeringHumansV1Action = runPostgresUserRepositoryV1 $ getAllHumansInOrganization engineersIdV1
  runReaderT engineeringHumansV1Action envV1 >>= print

  putStrLn "\nAnd in V2"

  let engineeringHumansV2Action = runPostgresUserRepositoryV2 $ getAllHumansInOrganization engineersIdV2
  runReaderT engineeringHumansV2Action envV2 >>= print

  pure ()
 where
  userId = HumanId $ Id 1

  engineersIdV1 = Id 2
  engineersIdV2 = Id 11

-- void $ runStateT modifyAndPrint []