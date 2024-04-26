{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core.Id (Id (Id))

import Control.Monad.Reader (void)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans
import Core.BankAccount (
  BankAccount (..),
  BankAccountHolderId (HumanId),
 )
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
  void $ runStateT modifyAndPrint []