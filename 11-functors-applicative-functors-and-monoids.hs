import Control.Applicative
import Data.Monoid

-- import our assertion function
import Assert

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

-- Make the list a Functor
instance Functor List where
  fmap _ Empty = Empty
  fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists as Empty = as
combineLists Empty bs = bs
combineLists (Value a as') bs = Value a (combineLists as' bs)

-- Make our list a Monoid
instance Monoid (List a) where
  mempty = Empty
  mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
  pure a = Value a Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Value f fs) <*> as = (f <$> as) <> (fs <*> as)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Monoid laws
testListObeysMonoidLaws :: IO()
testListObeysMonoidLaws =
  let
    xs = Value 1 (Value 2 (Value 3 (Value 4 Empty)))
    ys = Value 5 (Value 6 (Value 7 (Value 8 Empty)))
    zs = Value 9 (Value 10 (Value 11 (Value 12 Empty)))
  in do
    putStrLn "List should obey Monoid laws"
    assertEqual "(x <> y) <> z = x <> (y <> z)" ((xs <> ys) <> zs) (xs <> (ys <> zs))
    assertEqual "mempty <> x = x" xs (mempty <> xs)
    assertEqual "x <> mempty = x" xs (xs <> mempty)

-- Applicative laws
testListObeysApplicativeLaws :: IO()
testListObeysApplicativeLaws =
  let
    value = 1
    func = (+1)
    fs = Value (+1) (Value (+2) Empty)
    xs = Value 1 (Value 2 (Value 3 Empty))
  in do
    putStrLn "List should obey Applicative laws"
    assertEqual "pure id <*> v = v" xs (pure id <*> xs)
    assertEqual "pure f <*> pure x = pure (f x)" (pure (func value) :: List Int)
      (pure func <*> pure value :: List Int)
    assertEqual "u <*> pure y = pure ($ y) <*> u" (fs <*> pure value :: List Int)
      (pure ($ value) <*> fs :: List Int)
    assertEqual "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
      (pure (.) <*> fs <*> fs <*> xs) (fs <*> (fs <*> xs))
    -- Bonus!
    assertEqual "fmap f x = pure f <*> x" (fmap func xs) (pure func <*> xs)

-- Create some lists of numbers of different lengths such as:
threeValues = Value 1 $ Value 2 $ Value 3 Empty

fourValues = Value 4 $ Value 5 $ Value 6 $ Value 7 Empty

stringList = Value "foo" $ Value "bar" $ Value "baz" Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

threeValuesPlusTwo = plusTwo <$> threeValues

fourValuesPlusTwo = plusTwo <$> fourValues

-- Use <$> and <*> on the lists with a binary function
mul = (*)

threeValuesMultiplied = mul <$> threeValues <*> threeValues

fourValuesMultiplied = mul <$> fourValues <*> fourValues

stringListCombinations = (<>) <$> stringList <*> stringList

-- Create some lists of binary functions
binFuncs = Value (+) $ Value (*) $ Value (mod) Empty

-- Use <*> on the binary functions list and the number lists
threeValuesOnBinFuncs = binFuncs <*> threeValues <*> threeValues
