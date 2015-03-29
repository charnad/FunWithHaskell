import System.Environment
import Data.Char

import Euler.Basics
import Euler.Problems

main = do
    putStrLn "Project Euler solver"
    putStrLn "Viktoras Bezaras <charnad@gmail.com>"
    args <- getArgs
    processArguments args


processArguments :: [[Char]] -> IO ()
processArguments []       = putStrLn "Please provide a problem number as a single number"
processArguments (x:y:[]) = putStrLn "Please provide a problem number as a single number"
processArguments (x:[])
    | not (isNumericString x) = putStrLn "Argument is not numeric"
    | not (isSolved x)        = putStrLn "This problem has not been solved yet"
    | otherwise               = print (invokeProblem x) 


isNumericString :: [Char] -> Bool
isNumericString = and . map isNumber