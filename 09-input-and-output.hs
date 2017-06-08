{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import System.Environment
import System.Random
import Data.List

echo :: [String] -> IO ()
echo ("-n":rest) = putStr $ unwords rest
echo args = putStrLn $ unwords args

main :: IO ()
main = do
  args <- getArgs
  echo args

{-
 - Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = sort $ nub . take 6 $ randomRs (1, 49) gen

-- This will always return the same set of lottery tickets (numbers) as
-- we left the seed constant (referential transparency and all that)
myLotteryTickets = lottery $ mkStdGen 123
