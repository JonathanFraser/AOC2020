module Lib
    ( 
        day1part1,
        day1part2,
        day2
    ) where

import Control.Monad
import qualified Text.Parsec.Numbers as PN 
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec as PT

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


passwordParse = do 
                start <- PN.parseIntegral  
                PC.char '-'
                end <- PN.parseIntegral  
                PC.space
                char <- PC.letter 
                PC.char ':'
                PC.space 
                rest <- PT.many PC.alphaNum
                return (start,end,char,rest)


part1rule (s,e,c,str) = let 
                            count = length $ filter (==c) str 
                            in 
                                if count < s then 
                                     False
                                else 
                                    if count > e then 
                                        False 
                                    else 
                                        True

part2rule (i1,i2,c,str) = let 
                                pos1 = c == (str !! (i1-1))
                                pos2 = c == (str !! (i2-1))
                            in 
                                if pos1 && pos2 then 
                                    False 
                                else 
                                    if pos1 || pos2 then 
                                        True 
                                    else 
                                        False 


vetPassword :: MonadFail m => ((Int,Int,Char,String) -> Bool) -> String -> m Bool 
vetPassword f line = let 
                    out = PT.parse passwordParse "" line
                    in 
                        case out of 
                            Left r ->
                                fail $ show r 
                            Right v -> 
                                return $ f v
day2 :: IO ()
day2 = do
                values <- lines <$> readFile "inputs/inputd2p1.txt"
                part1 <- sequence $ fmap (vetPassword part1rule) values
                part2 <- sequence $ fmap (vetPassword part2rule) values 
                print  $ length $ filter id part1
                print $ length $ filter id part2 