module Accounting.Helpers
  ( module Accounting.Helpers
  , module Export
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad
import Data.Scientific
import Data.Coerce
import LocalPrelude
import Accounting
import Eesti as Export (nl, tekst)
import Eesti

-- * Generic (Prelude material)

partition'
  :: (Eq k, Hashable k, Monoid v)
  => (k -> v -> Bool) -> HM.HashMap k v -> (HM.HashMap k v, HM.HashMap k v)
partition' predicate hm = HM.foldlWithKey' f mempty hm
  where
    f (trues, falses) k v = if predicate k v
      then (HM.insertWith mappend k v trues, falses)
      else (trues, HM.insertWith mappend k v falses)

-- * Print

title :: String -> IO ()
title t = putStrLn $ t <> "\n"

h1 :: String -> IO ()
h1 t = title $ "* " <> t

h2 :: String -> IO ()
h2 t = title $ "** " <> t

h3 :: String -> IO ()
h3 t = title $ "*** " <> t

h4 :: String -> IO ()
h4 t = title $ "**** " <> t

rida :: Show a => String -> a -> IO ()
rida tekst väärtus = putStrLn $ tekst <> ": " <> show väärtus

ridaEur :: String -> Amount -> IO ()
ridaEur tekst v = putStrLn $ tekst <> ": " <> str <> " (" <> show v <> ")"
  where
    str = formatScientific Fixed (Just 0) $ coerce v

-- * Transactions

-- | List all used accounts
allAccounts :: [Transaction t a] -> [Account]
allAccounts ts = sort $ nubBy cmp $ do
  tr <- ts
  i <- tr^.instructions
  pure $ i^.account
  where
    cmp a b = a^.number == b^.number && a^.name == b^.name

accountType :: Account -> Int
accountType acc = case acc^.number.to (head . show) of
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5

bankMonth :: Account -> Integer -> Int -> [Transaction Day a] -> ((Amount, [Amount]), (Amount, [Amount]))
bankMonth acc y m ts = (sum' debits, sum' credits)
  where
    (debits, credits) = partition (\t -> t^._1.amount > 0) $ accountTransactions acc $ filter (selectYM y m) ts
    sum' d = let
      amounts = map (view $ _1.amount) d
      -- in (sum amounts) -- , amounts)
      in (sum amounts, amounts)
    selectYM :: HasTime s Day => Integer -> Int -> s -> Bool
    selectYM year' month' t = t^.time.year == year' && t^.time.month == month'

-- ** Saldo

saldoFilter :: (Account -> Bool) -> [Transaction t a] -> Amount
saldoFilter p ts = sumAmount $ map fst $ accountFilter p ts

-- | All debited instructions from account
saldoDebit :: Account -> [Transaction t a] -> [(Instruction, Transaction t a)]
saldoDebit acc ts = filter (\t -> t^._1.amount > 0) $ accountTransactions acc ts

-- | All credited instructions from account
saldoCredit :: Account -> [Transaction t a] -> [(Instruction, Transaction t a)]
saldoCredit acc ts = filter (\t -> t^._1.amount < 0) $ accountTransactions acc ts

-- ** Inside Out

byAccount :: [InsideOut t a] -> HM.HashMap Account [InsideOut t a]
byAccount io = HM.fromListWith (\t1 t2 -> t1 <> t2) [ (tup^._1.account, [tup]) | tup <- io]

-- * UI

printSaldo :: Account -> [Transaction t a] -> IO Amount
printSaldo acc ts = do
  putStr $ acc^.name <> " (" <> show (acc^.number) <> ")"
  putStr ": "
  let saldo' = saldo acc ts
  putStr $ show saldo'
  putStrLn " eur"
  return saldo'

printAccounts' :: (Show t, Show a) => Bool -> ByAccounts t a -> IO Amount
printAccounts' b byAccounts = let
  list = HM.toList byAccounts
  f total (acc, ios) = let
    sum = sumIOs ios
    in do
    rida ("- " <> acc^.name) sum
    when b $ print ios
    return $ total + sum
   in foldM f 0 list

printAccounts :: (Show t, Show a) => ByAccounts t a -> IO Amount
printAccounts = printAccounts' False

sumIOs :: [InsideOut t a] -> Amount
sumIOs ios = sum $ map (view (_1.amount)) ios

type ByAccounts t a = HM.HashMap Account [InsideOut t a]
type ByAccountTypes t a =
  ( ByAccounts t a
  , ByAccounts t a
  , ByAccounts t a
  , ByAccounts t a
  , ByAccounts t a)

toAccountTypes :: forall t a. (Show t, Show a) => [Transaction t a] -> ByAccountTypes t a
toAccountTypes ts = foldl f mempty $ insideOut ts
  where
    f :: ByAccountTypes t a -> InsideOut t a -> ByAccountTypes t a
    f r io
      | type_ == 1 = resultTo _1
      | type_ == 2 = resultTo _2
      | type_ == 3 = resultTo _3
      | type_ == 4 = resultTo _4
      | type_ == 5 = resultTo _5
      | otherwise = error $ "Exploding account" <> show io
      where
        type_ = io^._1.account.to accountType
        resultTo index = r & index %~ g io

    g :: InsideOut t a -> HM.HashMap Account [InsideOut t a] -> HM.HashMap Account [InsideOut t a]
    g io hm = HM.insertWith mappend (io^._1.account) (pure io) hm

calcKm :: Fractional b => b -> (b, b)
calcKm sum = (sum * 20 / 120, sum * 100 / 120)