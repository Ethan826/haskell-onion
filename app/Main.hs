{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader (void)
import Control.Monad.State (
  MonadState (get),
  StateT (runStateT),
  gets,
  modify,
 )
import Control.Monad.Trans
import Data.Foldable (find)

newtype Id = Id {getId :: Int}
  deriving (Show, Eq)

------------------------------------------------------------
-- Core layer
------------------------------------------------------------

data BankAccount = BankAccount
  { accountNumber :: Id
  , balanceCents :: Int
  , ownerId :: Id
  }
  deriving (Show, Eq)

------------------------------------------------------------
-- Services layer
------------------------------------------------------------

class (Monad m) => BankAccountPersistenceService m where
  createAccount :: BankAccount -> m ()
  getAccountById :: Id -> m (Maybe BankAccount)

------------------------------------------------------------
-- Providers layer
------------------------------------------------------------

type InMemoryBankAccountAction a = StateT [BankAccount] IO a

newtype InMemoryBankAccountPersistence a = InMemoryBankAccountPersistence
  { runInMemoryBankAccountPersistence :: InMemoryBankAccountAction a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState [BankAccount]
    , MonadIO
    )

instance BankAccountPersistenceService InMemoryBankAccountPersistence where
  createAccount :: BankAccount -> InMemoryBankAccountPersistence ()
  createAccount = modify . (:)

  getAccountById :: Id -> InMemoryBankAccountPersistence (Maybe BankAccount)
  getAccountById idToFind = gets $ find findById
   where
    findById :: BankAccount -> Bool
    findById = (== idToFind) . accountNumber

------------------------------------------------------------
-- Main
------------------------------------------------------------

someData :: BankAccount
someData =
  BankAccount
    { accountNumber = Id 123
    , balanceCents = 10000
    , ownerId = Id 987
    }

modifyAndPrint :: InMemoryBankAccountAction ()
modifyAndPrint = runInMemoryBankAccountPersistence $ do
  createAccount someData
  get >>= liftIO . print

main :: IO ()
main = do
  void $ runStateT modifyAndPrint []
