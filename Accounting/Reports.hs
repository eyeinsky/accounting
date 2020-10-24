module Accounting.Reports where

import qualified Data.HashMap.Lazy as HM

import LocalPrelude
import Accounting.Core

type InsideOut t a = (Instruction, Transaction t a)

type Balances t a = HM.HashMap Account [InsideOut t a]

insideOut :: [Transaction t a] -> [InsideOut t a]
insideOut ts = do
  tr <- ts
  i <- tr^.instructions
  pure (i, tr)

-- | For a list of transactions get a mapping from account to
-- instruction and its related transaction.
balances :: [Transaction t a] -> Balances t a
balances ts = ts
  & insideOut
  & map (\(i, tr) -> (i^.account, pure (i, tr)))
  & HM.fromListWith (\old new -> old <> new)

intervals :: forall t a b. Eq b => (Transaction t a -> b) -> [Transaction t a] -> [(b, Balances t a)]
intervals key ts = map (fmap balances) groups
  where
    groups' = groupBy ((==) `on` key) ts :: [[Transaction t a]]
    groups = map (\ts@ (t : _) -> (key t, ts)) groups'

vats :: Account -> Account -> [Transaction Day a] -> [((Integer, Int), Amount)]
vats vatIn vatOut ts = ret
  where
    kms :: [(Day, Amount)]
    kms = do
      (i, tr) <- insideOut ts
      let account' = i^.account
          amount' = i^.amount
          in_ = account' == vatIn && amount' > 0
          out = account' == vatOut && amount' < 0
      guard $ in_ || out
      pure (tr^.time, amount')

    kuu = groupBy (\a b -> mon (fst a) == mon (fst b))
    mon t = (t^.year, t^.month)

    kuud = kuu $ sortBy (compare `on` view _1) kms
    ret = map (\is@ ((k, _) : _) -> (mon k, sum $ map (view _2) is)) kuud
