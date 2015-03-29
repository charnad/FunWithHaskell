module Euler.Problems where

import Euler.Basics
import Data.List

solvedProblems = [
    ("1", problem1),
    ("2", problem2),
    ("3", problem3),
    ("4", problem4),
    ("5", problem5),
    ("6", problem6),
    ("7", problem7),
    ("9", problem9),
    ("10", problem10),
    ("14", problem14),
    ("16", problem16),
    ("20", problem20),
    ("21", problem21),
    ("23", problem23),
    ("24", problem24),
    ("25", problem25),
    --("29", problem29),
    ("30", problem30),
    ("34", problem34),
    --("35", problem35),
    ("36", problem36),
    ("37", problem37),
    --("39", problem39),
    ("41", problem41),
    ("44", problem44),
    --("45", problem45),
    --("47", problem47),
    ("48", problem48),
    ("52", problem52)
    ]

isSolved :: [Char] -> Bool
isSolved number = any ((== number) . fst) solvedProblems

invokeProblem :: [Char] -> Integer
invokeProblem number = head [snd t | t <- solvedProblems, (fst t) == number]

problem1 :: Integer
problem1 = sum [x | x <- [1..999], (mod x 3 == 0) || (mod x 5 == 0)]

problem2 :: Integer
problem2 = sum $ filter even (takeWhile (<= 4000000) fibonacciSequence)

problem3 :: Integer
problem3 = maximum $ primeFactors 600851475143

problem4 :: Integer
problem4 = maximum [x * y | x <- [100..999], y <- [x..999], isPalindrome (x * y)]

problem5 :: Integer
problem5 = head $ dropWhile (not . evenlyDivisible) [11 * 13 * 17 * 19 * 2520, 11 * 13 * 17 * 19 * 2521..]
    where evenlyDivisible x = null (dropWhile ((== 0).(mod x)) [1..20])

problem6 :: Integer
problem6 = ((^ 2).sum) [1..100] - (sum . (map (^2))) [1..100]

problem7 :: Integer
problem7 = primes !! 10000

problem9 :: Integer
problem9 = head [a * b * c | a <- [1..500], b <- [a..500], c <- [1000 - a - b], a ^ 2 + b ^ 2 == c ^ 2]

problem10 :: Integer
problem10 = sum (takeWhile (< 2000000) primes)

-- triangleNumberSequence :: [Integer]

-- problem12 :: Integer
-- problem12 = 

problem14 :: Integer
problem14 = fst $ foldl collatz (1, 0) [2..1000000]
      where collatzLength = (length . collatzSequence)
            collatz i x
                | collatzLength x > snd i = (x, collatzLength x)
                | otherwise = i

problem16 :: Integer
problem16 = (sum . digits) (2 ^ 1000)

problem20 :: Integer
problem20 = (sum . digits . factorial) 100
    where factorial n = if n == 1 then 1 else n * factorial (n - 1)

problem21 :: Integer
problem21 = sum [x | x <- [2..9999], x == (dFunc.dFunc) x, x /= dFunc x]
    where dFunc a = sum(properDivisors a)

problem23 :: Integer
problem23 = sum [x | x <- [2..9999], x == (dFunc.dFunc) x, x /= dFunc x]
    where dFunc a = sum(properDivisors a)

problem24' :: [Integer]
problem24' = abdundantNumbers
    where abdundantNumber n = n < (sum . properDivisors) n
          abdundantNumbers = filter abdundantNumber [3..28123]

problem24 :: Integer
problem24 = (fromDigits . (!! 999999) . sort . permutations) [0..9]

problem25 :: Integer
problem25 = head [fst p | p <- (zip [1..] fibonacciSequence), ((== 1000) . length . show . snd) p]

problem29 :: Int
problem29 = length $ foldl addUnique [] [a ^ b | a <- [2..100], b <- [2..100]]
    where addUnique l e = if (elem e l) then l else e:l

problem30 :: Integer
problem30 = sum $ [x | x <- [2..999999], x == (sum . map (^ 5) . digits) x]

problem34 :: Integer
problem34 = sum $ [x | x <- [3..999999], x == (sum . map factorial . digits) x]

problem35 :: Int
problem35 = length [x | x <- [2..1000000], isPrime x, and (map isPrime (rotates x))]
    where append num digit = num * 10 + digit
          rotate x = foldl append 0 ((last . digits) x : (init . digits) x)
          rotates n = take ((length . digits) n) (iterate rotate n)

problem36 :: Integer
problem36 = sum [x | x <- [1,3..1000000], isPalindrome x, (isPalindromeList . binary) x]

problem37 :: Integer
problem37 = (sum . take 11) $ filter (>10) [x | x <- primes, rightTruncatablePrime x, leftTruncatablePrime x]
    where rightTruncatablePrime n =  and (map isPrime (takeWhile (/=0) (iterate (flip div 10) n)))
          leftTruncatablePrime n = and (map isPrime (takeWhile (/=0) (iterate (fromDigits . tail . digits) n)))

problem39 :: [(Integer, Integer)]
problem39 = filter ((> 4).snd) [(p, rightTriangles p) | p <- [1..1000]]
    where rightTriangles p = genericLength [p | a <- [1..(div p 2)], b <- [a..(div p 2)], (a + b) < p, c <- [calcC p a b], ((a+b) + c) == p, (a^2 + b^2) == (c ^ 2)]
          calcC p a b = p - (a + b)

problem41 :: Integer
problem41 = maximum [ps | n <- [9,8..1], ps <- (map fromDigits (permutations [1..n])), isPrime ps]

problem44 :: Integer
problem44 = head [x-y | x <- p10, y <- p10, x > y, isPentagonal (x + y), isPentagonal (x - y)]
    where p10 = take 5000 $ drop 1000 pentagonalSequence

problem45 :: [Integer]
problem45 = take 3 [t | t <- triangleSequence, isPentagonal t, isHexagonal t]

problem47 :: [Integer]
problem47 = take 1 [x | x <- [1..], (genericLength . primeFactors) x == 4, (genericLength . primeFactors) (x + 1) == 4, (genericLength . primeFactors) (x + 2) == 4, (genericLength . primeFactors) (x + 3) == 4]

problem48 :: Integer
problem48 = mod (sum [x ^ x | x <- [1..1000]]) 10000000000

problem52 :: Integer
problem52 = head [x | x <- [1..], (f x (x*2)), (f x (x*3)), (f x (x*4)), (f x (x*5)), (f x (x*6))]
    where f x y = null ((show x) \\ (show y))