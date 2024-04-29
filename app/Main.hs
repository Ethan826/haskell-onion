{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core.Id (Id (Id))

import Control.Monad.Reader (forM_, void)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans
import Core.BankAccount (
  BankAccount (..),
  BankAccountHolderId (HumanId),
 )
import Database.PostgreSQL.Simple (Only (Only, fromOnly), connectPostgreSQL, query_)
import Providers.InMemoryBankAccountRepository (
  InMemoryBankAccountAction,
  InMemoryBankAccountRepository (runInMemoryBankAccountRepository),
 )
import Services.BankAccountRepository (
  BankAccountRepository (..),
 )

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

main :: IO ()
main = do
  conn <- connectPostgreSQL "postgresql://ethan@localhost:5432/ethan"
  xs :: [Only Int] <- query_ conn "SELECT id FROM users"
  forM_ xs (print . fromOnly)

-- forM_ xs $ \(id) -> print id

-- void $ runStateT modifyAndPrint []