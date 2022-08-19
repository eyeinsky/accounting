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

listUsedAccounts :: [Transaction t a] -> [Account]
listUsedAccounts ts = sort $ nubBy cmp $ do
  tr <- ts
  i <- tr^.instructions
  pure $ i^.account
  where
    cmp a b = a^.number == b^.number && a^.name == b^.name

-- | Group transactions @ts@ by years
tsByYear :: forall a . [Transaction Day a] -> [(Integer, [Transaction Day a])]
tsByYear [] = []
tsByYear ts@(_ : _) = go (earliestTr^.time.year) ts
  where
    earliestTr = minimumBy (compare `on` view time) ts

    go :: Integer -> [Transaction Day a] -> [(Integer, [Transaction Day a])]
    go year' ts = let
      (ts', rest) = partition (\t -> t^.time.year == year') ts
      in (year', ts') : go (year' + 1) rest
