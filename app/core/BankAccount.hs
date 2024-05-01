module Core.BankAccount where

import Core.Id (Id)
import Core.User (User)

type BankAccountId = Id BankAccount

data BankAccount = BankAccount
  { accountNumber :: BankAccountId
  , balanceCents :: Int
  , bankAccountUser :: User
  }
  deriving (Show, Eq)
