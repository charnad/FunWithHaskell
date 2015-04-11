module Euler.Problems where

import Euler.Basics
import Data.List

-- | A list of solved problems
solvedProblems = [
    ("1", problem1),
    ("2", problem2),
    ("3", problem3),
    ("4", problem4),
    ("5", problem5),
    ("6", problem6),
    ("7", problem7),
    ("8", problem8),
    ("9", problem9),
    ("10", problem10),
    ("12", problem12),
    ("14", problem14),
    ("16", problem16),
    ("20", problem20),
    ("21", problem21),
    ("24", problem24),
    ("25", problem25),
    ("29", problem29),
    ("30", problem30),
    ("34", problem34),
    ("35", problem35),
    ("36", problem36),
    ("37", problem37),
    --("39", problem39),
    ("41", problem41),
    ("44", problem44),
    ("45", problem45),
    ("47", problem47),
    ("48", problem48),
    ("52", problem52),
    ("55", problem55),
    ("502", problem502)
    ]

-- | Returns True is a problem is within solved
isSolved :: [Char] -> Bool
isSolved number = any ((== number) . fst) solvedProblems

-- | Invokes a problem by a number
invokeProblem :: [Char] -> Integer
invokeProblem number = head [snd t | t <- solvedProblems, (fst t) == number]

-- | Find the sum of all the multiples of 3 or 5 below 1000.
problem1 :: Integer
problem1 = sum [x | x <- [1..999], (mod x 3 == 0) || (mod x 5 == 0)]

-- | Find the sum of the even-valued Fibonacci terms, whose values do not exceed 4000000
problem2 :: Integer
problem2 = sum $ filter even (takeWhile (<= 4000000) fibonacciSequence)

-- | What is the largest prime factor of the number 600851475143 ?
problem3 :: Integer
problem3 = maximum $ primeFactors 600851475143

-- | Find the largest palindrome made from the product of two 3-digit numbers.
problem4 :: Integer
problem4 = maximum [x * y | x <- [100..999], y <- [x..999], isPalindrome (x * y)]

-- | What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
problem5 :: Integer
problem5 = head $ dropWhile (not . evenlyDivisible) [11 * 13 * 17 * 19 * 2520, 11 * 13 * 17 * 19 * 2521..]
    where evenlyDivisible x = all ((== 0).(mod x)) [1..20]

-- | Find the difference between the sum of the squares of one hundred and the square of the sum.
problem6 :: Integer
problem6 = ((^ 2).sum) [1..100] - (sum . (map (^2))) [1..100]

-- | What is the 10 001st prime number?
problem7 :: Integer
problem7 = primes !! 10000

-- | Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. 
problem8 :: Integer
problem8 = maximum $ filter (/= 0) (map (product . take 13) (tails number))
    where number = digits ((read::String->Integer) (
                   "73167176531330624919225119674426574742355349194934" ++
                   "96983520312774506326239578318016984801869478851843" ++
                   "85861560789112949495459501737958331952853208805511" ++
                   "12540698747158523863050715693290963295227443043557" ++
                   "66896648950445244523161731856403098711121722383113" ++
                   "62229893423380308135336276614282806444486645238749" ++
                   "30358907296290491560440772390713810515859307960866" ++
                   "70172427121883998797908792274921901699720888093776" ++
                   "65727333001053367881220235421809751254540594752243" ++
                   "52584907711670556013604839586446706324415722155397" ++
                   "53697817977846174064955149290862569321978468622482" ++
                   "83972241375657056057490261407972968652414535100474" ++
                   "82166370484403199890008895243450658541227588666881" ++
                   "16427171479924442928230863465674813919123162824586" ++
                   "17866458359124566529476545682848912883142607690042" ++
                   "24219022671055626321111109370544217506941658960408" ++
                   "07198403850962455444362981230987879927244284909188" ++
                   "84580156166097919133875499200524063689912560717606" ++
                   "05886116467109405077541002256983155200055935729725" ++
                   "71636269561882670428252483600823257530420752963450"))

-- | a^2 + b^2 = c^2, a + b + c = 1000
problem9 :: Integer
problem9 = head [a * b * c | a <- [1..500], b <- [a..500], c <- [1000 - a - b], a ^ 2 + b ^ 2 == c ^ 2]

-- | Find the sum of all the primes below two million.
problem10 :: Integer
problem10 = sum (takeWhile (< 2000000) primes)

-- | What is the value of the first triangle number to have over five hundred divisors?
problem12 :: Integer
problem12 = head [x | x <- triangularSequence, (length . divisors) x > 500]

-- | Which starting number, under one million, produces the longest chain?
problem14 :: Integer
problem14 = fst $ foldl collatz (1, 0) [2..1000000]
      where collatzLength = (length . collatzSequence)
            collatz i x
                | collatzLength x > snd i = (x, collatzLength x)
                | otherwise = i

-- | What is the sum of the digits of the number 21000?
problem16 :: Integer
problem16 = (sum . digits) (2 ^ 1000)

-- | Find the sum of the digits in the number 100!
problem20 :: Integer
problem20 = (sum . digits . factorial) 100
    where factorial n = if n == 1 then 1 else n * factorial (n - 1)

-- | Evaluate the sum of all the amicable numbers under 10000.
problem21 :: Integer
problem21 = sum [x | x <- [2..9999], x == (dFunc.dFunc) x, x /= dFunc x]
    where dFunc a = sum(properDivisors a)

problem24' :: [Integer]
problem24' = abdundantNumbers
    where abdundantNumber n = n < (sum . properDivisors) n
          abdundantNumbers = filter abdundantNumber [3..28123]

problem24 :: Integer
problem24 = (fromDigits . (!! 999999) . sort . permutations) [0..9]

problem25 :: Integer
problem25 = head [fst p | p <- (zip [1..] fibonacciSequence), ((== 1000) . length . show . snd) p]

problem29 :: Integer
problem29 = (genericLength . nub) [a ^ b | a <- [2..100], b <- [2..100]]

problem30 :: Integer
problem30 = sum $ [x | x <- [2..999999], x == (sum . map (^ 5) . digits) x]

problem34 :: Integer
problem34 = sum $ [x | x <- [3..999999], x == (sum . map factorial . digits) x]

-- | How many circular primes are there below one million?
problem35 :: Integer
problem35 = genericLength [x | x <- approvedPrimes, allPrimes (rotates x)]
    where approve x | x < 10 = True | otherwise = null $ intersect (digits x) [0, 2, 4, 5, 6, 8]
          -- Get all primes, that do not contain evens, they are not circular
          approvedPrimes = filter approve (takeWhile (< 1000000) primes)
          rotate x = fromDigits ((last . digits) x : (init . digits) x)
          rotates n = take ((length . show) n) (iterate rotate n)
          allPrimes p = p == (intersect p approvedPrimes)

problem36 :: Integer
problem36 = sum [x | x <- [1,3..1000000], isPalindrome x, (isPalindromeList . binary) x]

problem37 :: Integer
problem37 = (sum . take 11) $ filter (>10) [x | x <- primes, rightTruncatablePrime x, leftTruncatablePrime x]
    where rightTruncatablePrime n =  and (map isPrime (takeWhile (/=0) (iterate (flip div 10) n)))
          leftTruncatablePrime n = and (map isPrime (takeWhile (/=0) (iterate (fromDigits . tail . digits) n)))

-- | For which value of p ≤ 1000, is the number of right angle triangle solutions maximised?
problem39 :: [(Integer, Integer)]
problem39 = filter ((> 4).snd) [(p, rightTriangles p) | p <- [60,120..1000]]
    where rightTriangles p = genericLength [p | a <- [1..(div p 2)], b <- [a..(div p 2)], (a + b) < p, c <- [calcC p a b], ((a+b) + c) == p, (a^2 + b^2) == (c ^ 2)]
          calcC p a b = p - (a + b)

problem41 :: Integer
problem41 = maximum [ps | n <- [9,8..1], ps <- (map fromDigits (permutations [1..n])), isPrime ps]

problem44 :: Integer
problem44 = head [x-y | x <- p10, y <- p10, x > y, isPentagonal (x + y), isPentagonal (x - y)]
    where p10 = take 5000 $ drop 1000 pentagonalSequence

-- | Find the next triangle number that is also pentagonal and hexagonal
problem45 :: Integer
problem45 = [t | t <- hexagonalSequence, isPentagonal t, isTriangle t] !! 2

-- | Find the first of the first four consecutive integers to have four distinct prime factors
problem47 :: Integer
problem47 = head [x | x <- [1..], (genericLength . primeFactors) x == 4, (genericLength . primeFactors) (x + 1) == 4, (genericLength . primeFactors) (x + 2) == 4, (genericLength . primeFactors) (x + 3) == 4]

problem48 :: Integer
problem48 = mod (sum [x ^ x | x <- [1..1000]]) 10000000000

problem52 :: Integer
problem52 = head [x | x <- [1..], (f x (x*2)), (f x (x*3)), (f x (x*4)), (f x (x*5)), (f x (x*6))]
    where f x y = null ((show x) \\ (show y))

-- | How many Lychrel numbers are there below ten-thousand?
problem55 :: Integer
problem55 = genericLength [x | x <- [1..9999], isLychrel x]

-- | https://projecteuler.net/problem=502
problem502:: Integer
problem502 = castles 6 6
--problem502 = mod ((castles 1012 100) + (castles 10000 10000) + (castles 100 1012)) 1000000007