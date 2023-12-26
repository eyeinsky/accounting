module Accounting
  -- * Core, Reports
  ( Amount(Amount)
  , Transaction(..), T, I
  , Account(Account), number, time, name, account, amount
  , insideOut, accountTransactions
  , sumAmount, accountFilter
  , instructions
  , InsideOut, Instruction, HasTime

  -- * DSL
  , execT, execI, at, (~>), tsByYear
  , annotation
  , annotate

  -- * Text
  , nl, tekst', rida, tekst, ridaEur, h2, h3, h4

  -- And everything from this module
  , module Accounting
  )
  where

import LocalPrelude as Export
import Accounting.Core as Export
import Accounting.DSL as Export
import Accounting.Reports as Export
import Accounting.Print as Export

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

at :: Time t => t -> I ann a -> T t ann (Transaction t ann)
at = transact

-- * State

getAllTransactions :: Time t => T t ann [Transaction t ann]
getAllTransactions = do
  tParam <- get
  let
    singles' = toList $ tParam^.singles
    infinites' = tParam^.infinites
      & takeWhile (\t -> t^.time < (last singles'^.time))
  pure $ merge singles' infinites'

getBalance :: Time t => Account -> T t ann Amount
getBalance account = balance account <$> getAllTransactions

getBalanceFilter :: Time t => Account -> (Transaction t ann -> Bool) -> T t ann Amount
getBalanceFilter account p = balance account . filter p <$> getAllTransactions

-- ** Assert

assertBalance :: Time t => String -> Account -> Amount -> T t ann ()
assertBalance label account expected = do
  actual <- getBalance account
  when (actual /= expected) $ fail $ errMsg label account actual expected

type P t ann = Transaction t ann -> Bool

assertBalancesFilter :: Time t => String -> Account -> P t ann -> Amount -> T t ann ()
assertBalancesFilter label account p expected = do
  actual <- getBalanceFilter account p
  okOrFail (actual == expected) label $ errMsg label account actual expected

assertBalanceAt :: YearMonth -> String -> Account -> Amount -> T Day ann ()
assertBalanceAt ym = assertBalanceLens (time.yearMonth.to (== ym))

assertBalanceUntil :: YearMonth -> String -> Account -> Amount -> T Day ann ()
assertBalanceUntil ym = assertBalanceLens (time.yearMonth.to (<= ym))

-- | Assert @account@ balance to be @expected@, but filter the
-- transactions with @txBool@ getter.
assertBalanceLens
  :: Getting Bool (Transaction Day ann) Bool
  -> String -> Account -> Amount -> T Day ann ()
assertBalanceLens txBool label account expected = do
  actual <- getBalanceFilter account (view txBool)
  okOrFail (actual == expected) label $ errMsg label account actual expected

-- *** Helpers

okOrFail :: Bool -> String -> String -> T t ann ()
okOrFail cond trueMsg falseMsg = liftIO $ if cond
  then putStrLn $ "OK: " <> trueMsg
  else fail falseMsg

ok :: Time t => String -> T t ann ()
ok label = printMsg $ "OK: " <> label

errMsg :: String -> Account -> Amount -> Amount -> String
errMsg _label account actual expected = "Account "
  <> account^.name
  <> " has balance "
  <> show actual
  <> " instead of "
  <> show expected

-- ** Trace

-- | Report account balance via Debug.Trace. TODO: Use a type system
-- reified version of this instead.
traceBalance :: Time t => String -> Account -> T t ann ()
traceBalance label account = do
  balance <- getBalance account
  printMsg $ label <> ": " <> balanceStr account balance

traceBalance_ :: Time t => Account -> T t ann ()
traceBalance_ = traceBalance ""

traceBalances :: Time t => String -> [Account] -> T t ann ()
traceBalances label accounts = do
  accounts'balances <- mapM (\a -> (a,) <$> getBalance a) accounts
  labelBalancesStr label accounts'balances

traceBalancesFilter :: Time t => String -> [Account] -> P t ann -> T t ann ()
traceBalancesFilter label accounts p = do
  accounts'balances <- mapM (\a -> (a,) <$> getBalanceFilter a p) accounts
  labelBalancesStr label accounts'balances

traceBalanceAt :: (Integer, Int) -> [Account] -> T Day ann ()
traceBalanceAt ym accounts = do
  accounts'balances <- mapM (\a -> (a,) . balance a <$> getTransactionsAt ym) accounts
  labelBalancesStr ("traceBalanceAt " <> show ym) accounts'balances

traceBalanceUntil :: (Integer, Int) -> [Account] -> T Day ann ()
traceBalanceUntil ym accounts = do
  accounts'balances <- mapM (\a -> (a,) . balance a <$> getTransactionsUntil ym) accounts
  labelBalancesStr ("traceBalanceAt " <> show ym) accounts'balances

-- ** Helpers

balanceStr :: Account -> Amount -> String
balanceStr account balance =
     account^.number.to show <> ": "
  <> account^.name <> ": "
  <> show balance

labelBalancesStr label accounts'balances =
  printMsg $ label <> ": " <> concatMap (uncurry balanceStr) accounts'balances

getFilteredTx :: P Day ann -> T Day ann [Transaction Day ann]
getFilteredTx p = filter p <$> getAllTransactions

getTransactionsAt :: (Integer, Int) -> T Day ann [Transaction Day ann]
getTransactionsAt ym = getFilteredTx (\t -> t^.time.yearMonth == ym)

getTransactionsUntil :: (Integer, Int) -> T Day ann [Transaction Day ann]
getTransactionsUntil ym = getFilteredTx (\t -> t^.time.yearMonth <= ym)

printMsg :: Time t => String -> T t ann ()
printMsg = liftIO . putStrLn . ("Msg: " <>)
