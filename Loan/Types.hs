module Loan.Types where

import Prelude
import GHC.Real
import Data.Ord
import Control.Arrow

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

newtype Time = Time (Integer, Integer, Integer)
   deriving Eq

instance Ord Time where
   compare (Time (y, m, d)) (Time (y', m', d')) = let
      li = zipWith compare [y, m, d] [y', m', d']
      in case filter (/= EQ) li of
         a : _ -> a
         _ -> EQ

mkTime y m d = Time (y, m, d)
type DayInMonth = Integer

type Range = (Time, Time)
inRange t (start, end) = start <= t && t < end

nextMonth :: Time -> Time
nextMonth (Time t) = Time $ case t of
   (y, 12, p) -> (y + 1, 1, p)
   (y, m, p)  -> (y, m + 1, p)

type Amount = Double
type Percent = Rational
type Months = Integer

type Regime = Either Months Amount
years y = Left (y * 12)
monthly amount = Right amount
type Schedule = [(Time, Regime)]

data Payment = Payment
  { paymentInterest :: Amount
  , paymentPrincipal :: Amount
  } deriving (Show)

data State = State
  { statePrincipal :: Amount
  , statePayment :: Payment
  } deriving (Show)

type PaymentSchedule = [State]

totalExpense :: PaymentSchedule -> Amount
totalExpense = sum . map (\s -> let p = statePayment s in paymentPrincipal p + paymentInterest p)

prettyPaymentSchedule :: PaymentSchedule -> TL.Text
prettyPaymentSchedule states = TL.unlines $ map toRow (zip [(0 :: Int) ..] $ addInterestPaid states 0)
  where
    addInterestPaid (state : states) acc = let
        acc' = acc + paymentInterest (statePayment state)
      in (state, acc') : addInterestPaid states acc'
    addInterestPaid _ _ = []

    toRow (n, (State tp (Payment pi pp), interestPaid))
      = let
          x1 = show $ (+1) *** (+1) $ quotRem n 12
          x2 = show $ tp
          x3 = show $ pp + pi
          x4 = show $ pp
          x5 = show $ pi
          x6 = show $ interestPaid
      in TL.pack $ x1 <> ": " <> x2 <> " " <> x3 <> " (" <> x4 <> "/" <> x5 <> ") " <> x6

printPaymentSchedule :: PaymentSchedule -> IO ()
printPaymentSchedule = TLIO.putStrLn . prettyPaymentSchedule
