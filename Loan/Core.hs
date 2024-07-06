module Loan.Core
  ( module Loan.Core
  , module Loan.Types
  ) where

import Prelude
import Control.Arrow
import Control.Lens
import GHC.Real
import qualified Data.Text as T

import Loan.Types


data Loan
  = Loan { loanPrincipal :: Amount
         , loanPeriod    :: Integer -- ^ Period in units
         , loanInterest  :: Amount  -- ^ Interest per unit
         }

loanPayment :: Loan -> Amount
loanPayment (Loan p n i) = mortgage p (n * 12) (i / 12)

loanPaymentSchedule :: Loan -> PaymentSchedule
loanPaymentSchedule loan = tail $ iterate (step (loanPayment loan) (loanInterest loan)) initialState
  where
    initialState = State (loanPrincipal loan) (Payment 0 0)

-- | Calculate unit payment for a loan definition
mortgage :: Amount -> Integer -> Amount -> Amount
mortgage principal timeUnits interest = principal * (interest * x) / (x - 1)
  where
    x = _mortgageX interest timeUnits
    -- http://www.fonerbooks.com/interest.htm
    -- monthly payment M = P [ i (1 + i)^n ] / [ (1 + i)^n - 1]

_mortgageX :: Amount -> Integer -> Amount
_mortgageX interest units = (1 + interest) ** fromIntegral units


-- * Payment schedule

fromEventualTime :: Integer -> Amount -> Amount -> PaymentSchedule
fromEventualTime units interest principal = loanPaymentSchedule $ Loan principal units interest

fromFixedPayment :: Amount -> Amount -> Amount -> PaymentSchedule
fromFixedPayment monthlyPayment interest principal = tail $ iterate (step monthlyPayment interest) (State principal $ Payment 0 0)

step :: Amount -> Amount -> State -> State
step monthlyPayment interest state = let
   a = statePrincipal state
   i' = interest * a
   p' = monthlyPayment - i'
   r = a - p'
   in if r > 0
      then State r $ Payment i' p'
      else State 0 $ Payment i' a

-- * Calculation

payments
  :: (Time -> Amount)
  -> [(Time, Regime)]
  -> Amount
  -> PaymentSchedule
payments interAt (r : rs) principal' = untilZero $ go r rs principal'
   where
      go (ct, cr) regimes principal'' = case regimes of
         (nc@(nt, nr) : rs) -> let
               n = length $ takeWhile (< nt) $ iterate nextMonth ct
            in li <> go nc rs (statePrincipal . last $ li)
         _ -> li
         where
            interestRate = interAt ct
            f = either fromEventualTime fromFixedPayment cr
            li = f interestRate principal'' :: PaymentSchedule
payments _ _ _ = undefined

-- * Helpers

pr = mapM_ (print . f)
   where
      f = second (\t@(a,b) -> (a + b, t))

yearly :: Integer -> Amount -> Amount -> Amount
yearly yn yr s = mortgage s (yn * 12) (yr / 12)

untilZero :: PaymentSchedule -> PaymentSchedule
untilZero xs = as <> [b]
   where (as, b : _) = span ((> 0) . statePrincipal) xs
