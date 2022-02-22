{- In this sections you will implement the validation algorithm for credit 
cards.  It follows these steps:
	-Double the value of every second digit beginning from the right.  That is,
		the last digit is unchanged; the second-to-last digit is doubled; the 
		third to last digit is unchanged; and so on.  For example, [1,3,8,6]
		becomes [2,3,16,6].
	-Add the digits of the doubled values and the undoubled digits from the 
		original number.  For exapmle, [2,3,16,6] becomes 2+3+1+6+6 = 18.
	-Calculate the remainder when the sum is divided by 10.  For the above 
		example, the remainder would be 8.
		If the result equals 0, then the number is valid.
-}

-- Exercise 1: Find the digits of a number.  Define the functions toDigits and
-- toDigitsRev.  Comments below describe the respective functions.

-- Returns the digits of the input integer as a list in reverse order (i.e. the
-- ones digit will be the first element of the list, the tens digit will be the
-- second element, etc.).
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- Returns the digits of the input integer as a list (i.e. the ones digit will 
-- be the last element of the list, the tens digit will be the second to last
-- element, etc.).
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


-- Exercise 2: Double every other digit beginning from the right of the list.

-- Doubles every other number in the list starting from the right. That is, the
-- tail of the input list will not be doubled, and the second-to-last number 
-- will be doubled. 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther (x:[])     = [x]
doubleEveryOther (x:(y:zs))
  | mod (length zs) 2 == 0  = (2*x) : y : doubleEveryOther zs
  | otherwise               = x : (2*y) : doubleEveryOther zs
  
  
-- Exercise 3: Sum the digits of a list.

-- Sums the digits of each element in a list.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


-- Exercise 4: Indicate whether an Integer could be a valid credit card number.
-- This will use all the functions defined previously.

-- Validates whether an integer could be a valid credit card number
validate :: Integer -> Bool
validate n
  | mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0 = True
  | otherwise                                               = False
  

-- Exercise 5: Towers of Hanoi.  Move the disks from the first peg to the last 
-- peg.  Note that you can only move one disk at a time, and larger disks 
-- cannot be stacked on smaller disks.

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = (x, y):[]
hanoi n x y z =  ((hanoi (n-1) x z y) ++ [(x,y)]) ++ (hanoi (n-1) z y x)