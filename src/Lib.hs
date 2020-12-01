module Lib
    ( 
        day1part1,
        day1part2
    ) where

import Control.Monad


matchPair :: [a] -> ((a,a) -> Bool) -> [(a,a)]
matchPair xs chk = do 
                x <- xs
                y <- xs 
                guard (chk (x,y))
                return (x,y) 

readInts :: String -> IO [Int]
readInts fileName = do 
        content <- readFile fileName
        let splitLines = lines content 
        let values = map read splitLines :: [Int]
        return values 

day1part1 :: IO ()
day1part1 = do
            values <- readInts "inputs/inputd1p1.txt"
            let found = matchPair values (\(a,b) -> a+b == 2020)
            putStrLn $ show $ map (uncurry (*)) found

day1part2 :: IO ()
day1part2 = do 
            values <- readInts "inputs/inputd1p1.txt"
            let r = do 
                    a <- values 
                    b <- values 
                    c <- values 
                    guard (a+b+c == 2020)
                    return (a*b*c)
            putStrLn $ show $ r