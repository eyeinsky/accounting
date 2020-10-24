module Example where

import qualified Data.HashMap.Lazy as HM

import LocalPrelude
import Accounting

-- * Setup

-- ** Accounts

-- Equity (1)
bank, vatIn, cash, deptToMe, taxOfficePrepaid, vatOut, vatDebt, dept, capital, income, expenses, salary, socialTax :: Account
bank = Account 1 "Bank"
vatIn = Account 1 "VAT in"
cash = Account 1 "Cash"
deptToMe = Account 1 "Debt to me"
taxOfficePrepaid = Account 1 "Tax office prepaid"

-- Liabilities (2)
vatOut = Account 2 "VAT out"
vatDebt = Account 2 "VAT debt"
dept = Account 2 "Debt to other"

-- Capital (3)
capital = Account 3 "Capital"

-- Revenue (4)
income = Account 4 "Income"

-- Expenses (5)
expenses = Account 5 "Expenses"
salary = Account 5 "Salary"
socialTax = Account 5 "Social tax"

-- ** Annotation

data Annotation = Annotation { description :: String } deriving Show

instance Semigroup Annotation where
  _ <> a = a

instance Monoid Annotation where
  mempty = Annotation mempty

descr :: String -> I Annotation ()
descr text = annotate $ Annotation text

-- * Transactions

transactions :: [Transaction Day Annotation]
transactions = execT $ do

  at (stringDate "2020-01-15") $ do
    descr "Add initial equity"
    capital ~> bank $ 2500.00

  at (stringDate "2020-02-15") $ do
    descr "Provide services to company X"
    income ~> deptToMe $ 4000
    vatOut ~> deptToMe $ 800
    deptToMe ~> bank $ 4800

  at (stringDate "2020-02-20") $ do
    descr "Buy a screen from company Y"
    dept ~> expenses $ 300
    dept ~> vatIn $ 60
  at (stringDate "2020-02-06") $ do
    descr "Pay invoice: buy a monitor from company Y"
    bank ~> dept $ 360

  at (stringDate "2020-03-15") $ do
    descr "VAT declaration 2020-02"
    vatIn ~> vatDebt $ 60
    vatDebt ~> vatOut $ 800
    bank ~> taxOfficePrepaid $ 740
    taxOfficePrepaid ~> vatDebt $ 740

-- * VAT

monthlyVats :: [Transaction Day a] -> IO ()
monthlyVats ts = let
    key tr = (tr^.time.year, tr^.time.month)
    monthlyBalances = intervals key ts
    positiveSum = sum . filter (> 0) . map (view (_1.amount))
    negativeSum = sum . filter (< 0) . map (view (_1.amount))
  in forM_ monthlyBalances $ \((y, m), b) -> let
    vatIn' = fromMaybe [] $ HM.lookup vatIn b
    vatOut' = fromMaybe [] $ HM.lookup vatOut b
    in do
      putStrLn $ "\n" <> show y <> "-" <> show m <> ":"
      putStrLn $ "- VAT in: " <> show (positiveSum vatIn')
      putStrLn $ "- VAT out: " <> show (negativeSum vatOut')

-- * Report

sum' :: [InsideOut t a] -> Amount
sum' = sum . map (view (_1.amount))

main :: IO ()
main = do
  putStrLn "\nVAT, monthly:"
  monthlyVats transactions

  putStrLn "\nVAT, total:"
  let totals = balances transactions
  putStrLn $ "- VAT in: " <> (show $ maybe 0 sum' $ HM.lookup vatIn totals)
  putStrLn $ "- VAT out: " <> (show $ maybe 0 sum' $ HM.lookup vatOut totals)

  putStrLn "\nBalances:"
  forM_ totals $ \is@ ((i, _) : _) -> do
    putStrLn $ "- " <> i^.account.name <> ": " <> show (sum' is)
