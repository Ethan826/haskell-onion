{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core.Id (Id (Id))

import Control.Monad.Reader (void)
import Control.Monad.State (
  MonadState,
  StateT (runStateT),
  gets,
  modify,
 )
import Control.Monad.Trans
import Core.BankAccount (BankAccount (..), BankAccountHolderId (HumanId), BankAccountId)
import Data.Foldable (find)
import Services.BankAccountRepository (BankAccountRepository (..))

------------------------------------------------------------
-- Providers layer
------------------------------------------------------------

type InMemoryBankAccountAction a = Control.Monad.State.StateT [BankAccount] IO a

newtype InMemoryBankAccountRepository a = InMemoryBankAccountPersistence
  { runInMemoryBankAccountRepository :: InMemoryBankAccountAction a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState [BankAccount]
    , MonadIO
    )

instance BankAccountRepository InMemoryBankAccountRepository where
  createAccount :: BankAccount -> InMemoryBankAccountRepository ()
  createAccount = modify . (:)

  getAccountById ::
    BankAccountId ->
    InMemoryBankAccountRepository (Maybe BankAccount)
  getAccountById idToFind =
    gets $ find ((== idToFind) . accountNumber)

  getAccountsForUser ::
    BankAccountHolderId ->
    InMemoryBankAccountRepository [BankAccount]
  getAccountsForUser idToFind =
    gets (filter ((== idToFind) . bankAccountHolderId))

------------------------------------------------------------
-- Main
------------------------------------------------------------

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