module P017 where
-- * Question
-- If the numbers 1 to 5 are written out in words: one, two, three,
-- four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
-- total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were
-- written out in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three
-- hundred and forty-two) contains 23 letters and 115 (one hundred and
-- fifteen) contains 20 letters. The use of "and" when writing out
-- numbers is in compliance with British usage.

-- * Solution
-- We define a function that outputs the length of each number up to
-- 1000 accurately, then map and sum over it.
intLength :: Int -> Int
intLength 0 = 0
intLength 1 = 3
intLength 2 = 3
intLength 3 = 5
intLength 4 = 4
intLength 5 = 4
intLength 6 = 3
intLength 7 = 5
intLength 8 = 5
intLength 9 = 4
intLength 10 = length "ten"
intLength 11 = length "eleven"
intLength 12 = length "twelve"
intLength 13 = length "thirteen"
intLength 14 = length "fourteen"
intLength 15 = length "fifteen"
intLength 16 = length "sixteen"
intLength 17 = length "seventeen"
intLength 18 = length "eighteen"
intLength 19 = length "nineteen"
intLength 20 = length "twenty"
intLength 30 = length "thirty"
intLength 40 = length "forty"
intLength 50 = length "fifty"
intLength 60 = length "sixty"
intLength 70 = length "seventy"
intLength 80 = length "eighty"
intLength 90 = length "ninety"
intLength 1000 = length "onethousand"
intLength n
  | length n' == 2                                     = intLength (read [head n', '0'])
                                                         + intLength (read (tail n'))
  | length n' == 3 && n' !! 1 == '0' && n' !! 2 == '0' = intLength (read [head n'])
                                                         + length "hundred"
  | length n' == 3                                     = intLength (read [head n'])
                                                         + length "hundredand"
                                                         + intLength (read $ tail n')
    where
      n' = show n

p017 :: Int
p017 = sum . map intLength $ [1..1000]

main :: IO ()
main = print p017
