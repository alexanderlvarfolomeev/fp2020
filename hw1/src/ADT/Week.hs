{-# LANGUAGE LambdaCase #-}

module ADT.Week
  ( DayOfWeek (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

-- | 'DayOfWeek' data type represents of day of week
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

-- | @Enum@ instance according to days order in week
instance Enum DayOfWeek where
  fromEnum = \case
      Monday    -> 0
      Tuesday   -> 1
      Wednesday -> 2
      Thursday  -> 3
      Friday    -> 4
      Saturday  -> 5
      Sunday    -> 6
  toEnum = \case
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      x -> error $ "Can't convert " ++ show x ++ " to DayOfWeek"

-- | Equivalence of same days
instance Eq DayOfWeek where
  day1 == day2 = fromEnum day1 == fromEnum day2

-- | The 'nextDate' function returns the following 'DayOfWeek'
nextDay :: DayOfWeek -> DayOfWeek
nextDay = flip afterDays 1

-- | The 'afterDays' function returns 'DayOfWeek'
-- that will be past given number of days
afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day i = toEnum . (`mod` 7) . (+ i) . fromEnum $ day

-- | The 'isWeekend' function returns @True@ for weekend 'DayOfWeek'
-- and @False@ for workday
isWeekend :: DayOfWeek -> Bool
isWeekend day = day == Saturday || day == Sunday

-- | The 'daysToParty' function returns count of days until 'Friday'
daysToParty :: DayOfWeek -> Int
daysToParty = (`mod` 7) . (11 -) . fromEnum
