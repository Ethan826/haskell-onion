module Services.BankAccountRepository where

import Core.BankAccount (
  BankAccount,
  BankAccountHolderId,
  BankAccountId,
 )

class (Monad m) => BankAccountRepository m where
  createAccount :: BankAccount -> m ()
  getAccountById :: BankAccountId -> m (Maybe BankAccount)
  getAccountsForUser :: BankAccountHolderId -> m [BankAccount]
