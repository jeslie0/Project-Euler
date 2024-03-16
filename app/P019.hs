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
-- | A data type for the days of the week
data Days = Mon
          | Tue
          | Wed
          | Thu
          | Fri
          | Sat
          | Sun
          deriving (Eq, Show)

-- | We can enumerate the days of the week
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

-- | A datatype for the months in a year
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

-- | A type alias for the year
type Year = Int

-- | The month length, where the boolean input is True if and only if
-- the year is a leap year
monthLength :: Bool -> Month -> Int
monthLength _ Jan = 31
monthLength True Feb  = 29
monthLength False Feb = 28
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

-- | Given the starting day of the year, and whether or not it is a
-- leap year, output the number of sundays that start a month. We
-- calculate a list of the number of days in each month, modulo 7,
-- then form a list of the starting day of each month, from which we
-- count Sundays.
sundaysOnStartOfMonth :: Days -> Bool -> Int
sundaysOnStartOfMonth day bool =
  length . filter (== Sun) $
  scanl (\d n -> compose succ n d) day $
  map ((\n -> mod n 7) . monthLength bool) [Jan .. Nov]

-- | Gives the n-fold composite of f
compose :: (a -> a) -> Int -> a -> a
compose f 0 = \x -> x
compose f n = f . compose f (n-1)

-- | Generates a list of the year starting days and a boolean which
-- tells us if that year is a leap year or not
listOfStartDays :: [(Days, Bool)]
listOfStartDays = take 100 (genList' 1901 Mon)
  where
    genList' yr day
      | leapYear (yr - 1) = (compose succ (366 `mod` 7) day, leapYear yr)  : genList' (yr + 1) (compose succ (366 `mod` 7) day)
      | otherwise   = (compose succ (365 `mod` 7) day, leapYear yr) : genList' (yr + 1) (compose succ (365 `mod` 7) day)

-- | We map the number of sundaysOnStartOfMonth over the
-- listOfStartDays, then sum to get the final answer.
p019 :: Int
p019 = sum $ map (\(day,bool) -> sundaysOnStartOfMonth day bool) listOfStartDays

main :: IO ()
main = print p019
