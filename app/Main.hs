{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.Reader (Reader, ReaderT)

newtype Id = Id {getId :: Int}

------------------------------------------------------------
-- Core layer
------------------------------------------------------------

data BankAccount = BankAccount
  { accountNumber :: Id
  , balanceCents :: Int
  , ownerId :: Id
  }

------------------------------------------------------------
-- Services layer
------------------------------------------------------------

-- Reader Env for Postgres
data PostgresEnv = PostgresEnv {}

-- Marker type for the capacity to be used with database operations
class Persistable a

-- Declare that PostgresEnv implements Persistable
instance Persistable PostgresEnv

-- BankAccountPersistence type class for types a that are Persistable
class (Persistable a) => BankAccountPersistence a where
  createAccount :: BankAccount -> ReaderT a IO ()
  getAccountById :: Id -> ReaderT a IO BankAccount

main :: IO ()
main = putStrLn "Hello, Haskell!"
