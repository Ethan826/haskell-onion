{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.InMemoryBankAccountRepository where

import Control.Monad.State (
  MonadState,
  StateT,
  gets,
  modify,
 )
import Control.Monad.Trans
import Core.BankAccount (
  BankAccount (..),
  BankAccountId,
 )
import Core.User (UserId)
import Data.Foldable (find)
import Services.BankAccountRepository (BankAccountRepository (..))

type InMemoryBankAccountAction a = StateT [BankAccount] IO a

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
    UserId ->
    InMemoryBankAccountRepository [BankAccount]
  getAccountsForUser idToFind =
    gets (filter ((== idToFind) . bankAccountHolderId))
