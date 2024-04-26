module Core.BankAccount where

import Core.Human (HumanId)
import Core.Id (Id)
import Core.Organization (OrganizationId)

type BankAccountId = Id BankAccount

data BankAccount = BankAccount
  { accountNumber :: BankAccountId
  , balanceCents :: Int
  , bankAccountHolderId :: BankAccountHolderId
  }
  deriving (Show, Eq)

data BankAccountHolderId
  = HumanId HumanId
  | OrganizationId OrganizationId
  deriving (Show, Eq)