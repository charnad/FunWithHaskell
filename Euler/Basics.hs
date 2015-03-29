module Euler.Basics where

import Data.List

dividers :: Integer -> [Integer]
dividers n = [2..ceiling(sqrt(fromIntegral n))]

divisors :: Integer -> [Integer]
divisors n = smallDividers ++ [(div n divider) | divider <- smallDividers, (not (elem (div n divider) smallDividers)) ]
    where smallDividers = filter (\x -> (mod n x) == 0) (dividers n)

properDivisors :: Integer -> [Integer]
properDivisors n = 1 : divisors n

isPalindromeList :: [Integer] -> Bool
isPalindromeList list = list == reverse list

isPalindrome :: Integer -> Bool
isPalindrome n | n < 10 = True
isPalindrome n = (isPalindromeList . digits) n

digits :: Integer -> [Integer]
digits n | n < 10 = [n]
digits n = digits (div n 10) ++ [mod n 10]

fromDigits :: [Integer] -> Integer
fromDigits list = foldl append 0 list
    where append num digit = num * 10 + digit

smallestCommonFactor :: Integer
smallestCommonFactor = head $ filter divisible [2520*step, 2521*step..((product [11..20])*2520)]
    where step = product (filter isPrime [11..20])
          divisible x = null (filter (/= 0) (map ($ x) [(flip mod) x | x <- [1..20]]))

factorial :: Integer -> Integer
factorial n = product [1..n]

fibonacciSequence :: [Integer]
fibonacciSequence = 1 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all ((/= 0).(mod n)) [2..ceiling(sqrt(fromIntegral n))]

primes :: [Integer]
primes = filter isPrime [2..]

count :: (Eq a) => [a] -> a -> Int
count l e = length $ filter (==e) l

binary :: Integer -> [Integer]
binary 0 = [0]
binary 1 = [1]
binary x = binary (div x 2) ++ [mod x 2]

collatzSequence :: Integer -> [Integer]
collatzSequence n | n == 1 = [1]
                  | even n = n : collatzSequence (div n 2)
                  | otherwise = n : collatzSequence ((3 * n) + 1)


pentagonalSequence :: [Integer]
pentagonalSequence = [div (x*(3*x - 1)) 2 | x <- [1..]]

isPentagonal :: Integer -> Bool
isPentagonal x = x == (head (dropWhile (< x) pentagonalSequence))

triangleSequence :: [Integer]
triangleSequence = [div (n * (n + 1)) 2 | n <- [1..]]

isTriangle :: Integer -> Bool
isTriangle x = x == (head (dropWhile (< x) triangleSequence))

hexagonalSequence :: [Integer]
hexagonalSequence = [n * (2 * n - 1) | n <- [1..]]

isHexagonal :: Integer -> Bool
isHexagonal x = x == (head (dropWhile (< x) hexagonalSequence))

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors 2 = [2]
primeFactors n = nub ((nextFactor n) : primeFactors (div n (nextFactor n)))
    where nextFactors n = dropWhile ((/= 0).(mod n)) (takeWhile (<= n) primes)
          nextFactor n = if (null (nextFactors n)) then 1 else head (nextFactors n)