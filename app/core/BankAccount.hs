module Core.BankAccount where

import Core.Id (Id)
import Core.User (UserId)

type BankAccountId = Id BankAccount

data BankAccount = BankAccount
  { accountNumber :: BankAccountId
  , balanceCents :: Int
  , bankAccountHolderId :: UserId
  }
  deriving (Show, Eq)
