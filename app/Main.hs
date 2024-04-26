{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader (void)
import Control.Monad.State (
  MonadState,
  StateT (runStateT),
  gets,
  modify,
 )
import Control.Monad.Trans
import Data.Foldable (find)
import Data.Text (Text)

-- Define IDs for different entities
newtype Id a = Id Int deriving (Show, Eq, Ord)

type HumanId = Id Human
type OrganizationId = Id Organization
type BankAccountId = Id BankAccount

data AccountHolderId
  = HumanId HumanId
  | OrganizationId OrganizationId
  deriving (Show, Eq)

-- Define data structures
data BankAccount = BankAccount
  { accountNumber :: BankAccountId
  , balanceCents :: Int
  , accountHolderId :: AccountHolderId
  }
  deriving (Show, Eq)

data Human = Human
  { humanName :: Text
  , humanId :: HumanId
  }
  deriving (Show, Eq)

data Organization = Organization
  { organizationName :: Text
  , organizationId :: OrganizationId
  , organizationHumanIds :: [HumanId]
  }
  deriving (Show, Eq)

data User
  = HumanUser Human
  | OrganizationUser Organization
  deriving (Show, Eq)

class (Monad m) => UserRepository m where
  createUser :: User -> m ()
  getUserById :: HumanId -> m (Maybe User)
  getAllHumansInOrganization :: OrganizationId -> m [Human]

class (Monad m) => BankAccountRepository m where
  createAccount :: BankAccount -> m ()
  getAccountById :: BankAccountId -> m (Maybe BankAccount)
  getAccountsForUser :: AccountHolderId -> m [BankAccount]

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
    AccountHolderId ->
    InMemoryBankAccountRepository [BankAccount]
  getAccountsForUser idToFind =
    gets (filter ((== idToFind) . accountHolderId))

------------------------------------------------------------
-- Main
------------------------------------------------------------

someData :: BankAccount
someData =
  BankAccount
    { accountNumber = Id 123
    , balanceCents = 10000
    , accountHolderId = HumanId $ Id 987
    }

modifyAndPrint :: InMemoryBankAccountAction ()
modifyAndPrint = runInMemoryBankAccountRepository $ do
  createAccount someData
  account <- getAccountById (Id 123)
  liftIO $ print account

main :: IO ()
main = do
  void $ runStateT modifyAndPrint []