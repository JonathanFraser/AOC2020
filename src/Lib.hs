module Lib
    ( 
        day1,
        day2, 
        day3
    ) where

import Control.Monad
import Data.Maybe
import qualified Text.Parsec.Numbers as PN 
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec as PT
import qualified Data.Map as Map 

findSum :: (Eq a,Num a) => Int -> a -> [a] -> [[a]]
findSum 0 _ _ = []
findSum _ _ [] = []
findSum 1 m xs = map (:[]) $ filter (==m) xs
findSum n m (x:xs) = (findSum n m xs) ++ do 
                                            y <- findSum (n-1) (m-x) xs
                                            return (x:y)

readInts :: String -> IO [Int]
readInts fileName = do 
        content <- readFile fileName
        let splitLines = lines content 
        let values = map read splitLines :: [Int]
        return values

day1 :: IO ()
day1 = do 
        values <- readInts "inputs/inputd1p1.txt"
        print $ map product $ findSum 2 2020 values
        print $ map product $ findSum 3 2020 values

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
                                (count >= s) && (count <= e)

part2rule (i1,i2,c,str) = let 
                                pos1 = c == (str !! (i1-1))
                                pos2 = c == (str !! (i2-1))
                            in 
                               (pos1 || pos2) && not (pos1 && pos2) 


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
                part1 <- mapM (vetPassword part1rule) values
                part2 <- mapM (vetPassword part2rule) values 
                print  $ length $ filter id part1
                print $ length $ filter id part2 

parseSquare :: Char -> Bool
parseSquare '#' = True 
parseSquare '.' = False  

parseLine :: Int -> String -> Map.Map (Int,Int) Bool
parseLine rowidx chars = Map.fromList $ map (\(i,c) -> ((rowidx,i),parseSquare c)) $ zip [0..] chars

parseMap :: [String] -> Map.Map (Int,Int) Bool 
parseMap lines = foldl (\m (i,l) -> Map.union m $ parseLine i l ) Map.empty $ zip [0..] lines 

getMax :: Map.Map (Int,Int) a -> (Int,Int) 
getMax d = let 
            ks = Map.keys d
            (l,r) = unzip ks
            in (maximum l +1 ,maximum r+1)



pathgen :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
pathgen (height,width) (v,h) = let 
                                    verts = takeWhile (<height) $ iterate ((+) v) 0 
                                    hortz = map (\x -> mod x width) $ iterate ((+) h) 0 
                                in zip verts hortz 

testPath :: Map.Map (Int,Int) Bool -> [(Int,Int)] -> Maybe Int
testPath mountain coords = do 
            let istree = map (\ x -> Map.lookup x mountain) coords  
            let remlist = mapMaybe (id) istree
            guard (length istree == length remlist)
            return $ length $ filter (id) remlist


day3 :: IO ()
day3 = do 
        values <- lines <$> readFile "inputs/inputd3.txt"
        let mountain = parseMap values 
        let dim = getMax mountain
        let comp = (\x -> testPath mountain $ pathgen dim x)
        let slps = [(1,3),(1,1),(1,5),(1,7),(2,1)]
        let res = map comp slps
        print $ head res 
        print res 
        print $ product $ mapMaybe (id) res
