module Data.Time.Lens where

import Prelude
import Data.Time
import Data.Time.Calendar.WeekDate
import Control.Lens


day :: Lens' Day Integer
day f (ModifiedJulianDay a) = fmap (\a' -> ModifiedJulianDay a') (f a)

mday :: Lens' Day Int
mday f a = fmap (fromGregorian y m) (f d)
  where
    (y, m, d) = toGregorian a
-- ^ fix: Overflow handling depends on the implementation of toGregorian

month :: Lens' Day Int
month f a = fmap mk (f m)
  where
    (y, m, d) = toGregorian a
    mk m' = let
        (dy, m'') = (m' - 1) `divMod` 12
      in fromGregorian (y + toInteger dy) (m'' + 1) d

year :: Lens' Day Integer
year f a = fmap mk (f y)
  where
    (y, m, d) = toGregorian a
    mk y' = fromGregorian y' m d

weekday :: Lens' Day DayOfWeek
weekday f a = fmap mk (f $ toEnum $ i - 1)
  where
    mk wd = fromWeekDate y yw (fromEnum wd + 1)
    (y, yw, i) = toWeekDate a

weeknumber :: Lens' Day Int
weeknumber f a = fmap mk (f yw)
  where
    mk yw' = fromWeekDate y yw' wdi
    (y, yw, wdi) = toWeekDate a

workday :: DayOfWeek -> Bool
workday wd = fromEnum wd < 5

-- | Can only get, not set, hence function
lastDay :: Day -> Day
lastDay d = d
  & year %~ (+1)
  & mday .~ 1
  & month .~ 1
  & day %~ (\n -> n - 1)
