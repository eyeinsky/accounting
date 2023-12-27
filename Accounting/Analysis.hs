module Accounting.Analysis where

import qualified Data.HashMap.Lazy as HM

import LocalPrelude
import Accounting.Core


type InsideOut t a = (Instruction, Transaction t a)

-- | All instructions for account
accountFilter :: (Account -> Bool) -> [Transaction t a] -> [InsideOut t a]
accountFilter predicate ts = filter (\tup -> predicate $ tup^._1.account) $ insideOut ts

-- | Balance (sum of debits and credits) for a single account
balance :: Account -> [Transaction t a] -> Amount
balance acc ts = sumAmount $ map fst $ accountFilter (== acc) ts
  where
    sumAmount :: [Instruction] -> Amount
    sumAmount is = foldl (\n i -> n + (i^.amount)) 0 is

-- | Balance sheet from InsideOut. The tx/InsideOut must already be filtered appropriately.
balanceSheet :: [InsideOut Day ann] -> HM.HashMap Account Amount
balanceSheet ios = HM.fromListWith (+) $ do
  (ix, tx) <- ios
  pure (ix ^. account, ix ^. amount)

-- * Journal

-- | Print all events for a single account in format: tx.date, ix.amount, tx.description
accountJournal :: Account -> [Transaction Day String] -> IO ()
accountJournal account_ ts = ts
  & sortBy (compare `on` view time)
  & insideOut
  & filter (fst ^ view account ^ eq account_)
  & mapM_ printLn
  where
    printLn (ix, tx) = putStrLn $ printf "%s %6s %s"
      (tx^.time.to show)
      (ix^.amount.to (fixedSpaceAmount 2))
      (tx^.annotation)

    fixedSpaceAmount :: Int -> Amount -> String
    fixedSpaceAmount digits amount = formatScientific Fixed (Just digits) (coerce amount)


-- old

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
    groups = map (\ts@(t : _) -> (key t, ts)) groups'

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
    ret = map (\is@((k, _) : _) -> (mon k, sum $ map (view _2) is)) kuud
