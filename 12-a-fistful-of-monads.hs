-- 12-a-fistful-of-monads.hs

 -- Create a type called Validation
 -- The type constructor takes one parameter
 -- There are two Values:
 --   Success, which takes that parameter and
 --   Fail String, which represents a failure, and a reason for that failure

data Validation a = Success a | Fail String
  deriving (Show, Eq)

-- Make the Validation a Monad

instance Functor Validation where
  fmap _ (Fail err) = Fail err
  fmap f (Success x) = Success (f x)

instance Applicative Validation where
  pure = Success

  Fail err <*> _ = Fail err
  Success f <*> v = fmap f v

instance Monad Validation where
  return = Success

  Fail err >>= _ = Fail err
  (Success x) >>= f = f x

-- Create a function, positiveCheck, which takes a number and returns a
-- successful Validation if it's positive, and a failed Validation with
-- a String message if not.
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x = if x >= 0 then Success x else Fail "Not positive!"

-- Create a function, evenCheck, which returns a successful Validation
-- if it's even, and a failed Validation with a string message if it's
-- odd.
evenCheck :: (Integral a) => a -> Validation a
evenCheck x = if x `mod` 2 == 0 then Success x else Fail "Not even!"

-- Write a function which uses positiveCheck and evenCheck to make sure
-- a number is both positive and even
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = return x >>= positiveCheck >>= evenCheck
