module Services.BankAccountRepository where

import Core.BankAccount (
  BankAccount,
  BankAccountId,
 )
import Core.User (UserId)

class (Monad m) => BankAccountRepository m where
  createAccount :: BankAccount -> m ()
  getAccountById :: BankAccountId -> m (Maybe BankAccount)
  getAccountsForUser :: UserId -> m [BankAccount]
