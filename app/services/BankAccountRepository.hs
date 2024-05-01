module Services.BankAccountRepository where

import Core.BankAccount (
  BankAccount,
  BankAccountId,
 )

-- import Core.User (UserId)

class (Monad m) => BankAccountRepository m where
  -- createAccount :: BankAccount -> m ()
  -- getAccountsForUser :: UserId -> m [BankAccount]
  getAccountById :: BankAccountId -> m (Maybe BankAccount)
