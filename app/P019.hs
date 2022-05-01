module P019 where

-- * Question
-- You are given the following information, but you may prefer to do
-- some research for yourself.
--
-- -   1 Jan 1900 was a Monday.
-- -   Thirty days has September,
-- -   April, June and November.
-- -   All the rest have thirty-one,
-- -   Saving February alone,
-- -   Which has twenty-eight, rain or shine.
-- -   And on leap years, twenty-nine.
-- -   A leap year occurs on any year evenly divisible by 4, but not
--     on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the
-- twentieth century (1 Jan 1901 to 31 Dec 2000)?
-- * Solution

data Days = Mon
          | Tue
          | Wed
          | Thu
          | Fri
          | Sat
          | Sun
          deriving (Eq, Show)

instance Enum Days where
  fromEnum Mon = 1
  fromEnum Tue = 2
  fromEnum Wed = 3
  fromEnum Thu = 4
  fromEnum Fri = 5
  fromEnum Sat = 6
  fromEnum Sun = 7

  toEnum n
    | n == 1 = Mon
    | n == 2 = Tue
    | n == 3 = Wed
    | n == 4 = Thu
    | n == 5 = Fri
    | n == 6 = Sat
    | n == 0 = Sun
    | otherwise = toEnum (n `mod` 7)

nextDay :: Days -> Days
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec
           deriving (Eq, Show, Enum)

type Year = Int

monthLength :: Year -> Month -> Int
monthLength _ Jan = 31
monthLength yr Feb
  | leapYear yr = 29
  | otherwise   = 28
monthLength _ Mar = 31
monthLength _ Apr = 30
monthLength _ May = 31
monthLength _ Jun = 30
monthLength _ Jul = 31
monthLength _ Aug = 31
monthLength _ Sep = 30
monthLength _ Oct = 31
monthLength _ Nov = 30
monthLength _ Dec = 31

-- | A predicate determining if a year is a leap year
leapYear :: Year -> Bool
leapYear n
  | n `mod` 400 == 0 = True
  | n `mod` 100 == 0 = False
  | n `mod` 4 == 0   = True
  | otherwise        = False

startDay :: Year -> Days
startDay 1900 = Mon
startDay n
  | leapYear n = succ . succ . startDay $ n - 1
  | otherwise  = succ . startDay $ n - 1

-- sundaysOnStartOfMonth :: Year -> Int
-- sundaysOnStartOfMonth yr
--   | leapYear yr = _
--   | otherwise = _

-- p019 =

main = print "hi"
