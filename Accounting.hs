module Accounting
  ( module Export
  , module Accounting
  ) where

import Accounting.Core as Export
import Accounting.DSL as Export
import Accounting.Reports as Export

-- * Helpers

-- | All instructions for account
accountFilter :: (Account -> Bool) -> [Transaction t a] -> [(Instruction, Transaction t a)]
accountFilter predicate ts = filter (\tup -> predicate $ tup^._1.account) $ insideOut ts

-- | All instructions for account
accountTransactions :: Account -> [Transaction t a] -> [(Instruction, Transaction t a)]
accountTransactions acc ts = accountFilter (== acc) ts

sumAmount :: [Instruction] -> Amount
sumAmount is = foldl (\n i -> n + (i^.amount)) 0 is

-- | Sum for account
balance :: Account -> [Transaction t a] -> Amount
balance acc ts = sumAmount $ map fst $ accountTransactions acc ts
