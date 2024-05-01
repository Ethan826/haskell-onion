{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.SqliteBankAccountRepository where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks)
import Core.BankAccount (BankAccount (BankAccount, accountNumber, balanceCents, bankAccountUser), BankAccountId)
import Core.Id (Id (Id, unId))
import Core.User (UserId (HumanId, OrganizationId))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (open, query)
import Services.BankAccountRepository (BankAccountRepository (getAccountById))
import Services.UserRepository (UserRepositoryActions (..))

instance BankAccountRepository SqliteBankAccountRepository where
  getAccountById :: BankAccountId -> SqliteBankAccountRepository (Maybe BankAccount)
  getAccountById accountId = do
    file <- asks sqliteFileLocation
    UserRepositoryActions{runGetUserById} <- asks userRepositoryActions
    conn <- liftIO $ open file
    result :: [(Int, Int, Text)] <-
      liftIO $
        query
          conn
          "SELECT balance_cents, user_id, user_type FROM accounts WHERE id = (?);"
          [unId accountId]
    case listToMaybe result of
      Just (cents, userId, userType) -> do
        let userTypeToId = case userType of
              "human" -> Just $ HumanId $ Id userId
              "organization" -> Just $ OrganizationId $ Id userId
              _ -> Nothing -- Should be an error
        case userTypeToId of
          Just userId' -> do
            user <- liftIO $ runGetUserById userId'
            pure $ createBankAccount <$> user
          Nothing -> pure Nothing
       where
        createBankAccount user =
          BankAccount
            { accountNumber = accountId
            , balanceCents = cents
            , bankAccountUser = user
            }
      Nothing -> pure Nothing

data SqliteBankAccountRepositoryEnv = SqliteBankAccountRepositoryEnv
  { sqliteFileLocation :: String
  , userRepositoryActions :: UserRepositoryActions
  }

type SqliteBankAccountRepositoryAction a = ReaderT SqliteBankAccountRepositoryEnv IO a

newtype SqliteBankAccountRepository a = SqliteBankAccountRepository
  { runSqliteBankAccountRepository :: SqliteBankAccountRepositoryAction a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SqliteBankAccountRepositoryEnv
    , MonadIO
    )
