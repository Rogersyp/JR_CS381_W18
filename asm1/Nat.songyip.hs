--- Implemented by
--      * Yipeng Song    932-470-819
--      * Jeremy Fischer 932-447-681

module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ num) = num


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ num) = 1 + (toInt num)


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   

-- How It Works: The function recursivly adds the two numbers
-- together by exploiting that (num1 + 1) + (num2 - 1) == num1 + num2.
-- So, the function does this until the base case is met where one of the numbers
-- is Zero. (add num Zero = num).
-- Example: add two two == add (Succ two) one --> 
--			add three one == add (Succ three) Zero -->
--			add four Zero == four == Succ (Succ (Succ (Succ Zero)))
add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero num = num
add num Zero = num
add num (Succ num2) = add (Succ num) num2


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--

-- How It Works: The function recursivly subtracts the two numbers
-- together by exploiting that num1 - num2 == (num1 - 1) - (num2 - 1).
-- So, the function does this until the base case is met where one of the numbers
-- is Zero. (sub num Zero = num).
-- Example: sub four two == sub (Succ three) (Succ one) --> 
--			sub three one == sub (Succ two) (Succ Zero) -->
--			sub two Zero == two == (Succ (Succ Zero))
sub :: Nat -> Nat -> Nat
sub Zero Zero = Zero
sub Zero num = Zero
sub num Zero = num
sub (Succ num) (Succ num2) = sub num num2


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--

-- How It Works: This function utilizes the same algorithm
-- as sub does. The numbers are recursively reduced by one 
-- until one of them is Zero. If the number on the left is
-- zero first, then the right number is LESS than the right number.
-- If the number on the right is zero first, then the left
-- number is GREATER than the right number
gt :: Nat -> Nat -> Bool
gt Zero Zero = False
gt Zero num = False
gt num Zero = True
gt (Succ num) (Succ num2) = gt num num2

-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult num1 (Succ num2) = add num1 (mult num1 num2)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero
sum (h:t) = add h (sum t)


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = one : map (add two) odds
