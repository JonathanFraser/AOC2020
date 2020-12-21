module Lib
    ( 
        day1,
        day2, 
        day3,
        day4,
        day5,
        day6,
        day7,
        day8,
        day9,
        day10,
        day11,
        day12,
        day13,
        day14,
        day15,
        day16,
        day17,
        day18,
        day19
    ) where

import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Either
import Data.List 

import qualified Data.Array as Array 
import qualified Data.Bits as Bits 
import qualified Data.Set as Set 
import qualified Data.List as List 
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

packPassport stuff = 
                let 
                    (start, end) = break (\x -> x=="" || x == "\r") stuff
                    list = words $ List.intercalate " " start 
                in 
                    if end == [] then 
                        [list]
                    else 
                        list:packPassport (tail end)

parseYear :: PT.ParsecT [Char] st Identity Int
parseYear = do 
            yearstr <- PT.count 4 PT.digit
            let year = read yearstr
            return year 

data Units = Inches Int | CM Int deriving Show

data EYE = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving Show 
data Field = BYR Int | IYR Int | EYR Int | HGT Units | CLR String | ECL EYE | PID String | CID deriving Show

colon :: PT.ParsecT [Char] st Identity Char
colon=PC.char ':'

parseBirthYear = do 
                    PT.try $ PC.string "byr"
                    colon 
                    year <- parseYear 
                    guard (year >= 1920)
                    guard (year <= 2002)
                    return $ Map.singleton "byr" (BYR year)


parseIssueYear = do 
                    PT.try $ PC.string "iyr"
                    colon
                    year <- parseYear 
                    guard (year >= 2010)
                    guard (year <= 2020)
                    return $ Map.singleton "iyr" (IYR year)

parseExpirationYear = do 
                        PT.try $ PC.string "eyr"
                        colon
                        year <- parseYear
                        guard (year >= 2020)
                        guard (year <= 2030)
                        return $ Map.singleton "eyr" (EYR year)




parseCM = do 
            val <- PN.parseIntegral 
            PC.string "cm"
            guard (val >= 150)
            guard (val <= 193)
            return (CM val)


parseInches = do 
            val <- PN.parseIntegral 
            PC.string "in"
            guard (val >= 59)
            guard (val <= 76)
            return (Inches val)


parseHeight = do 
                PT.try $ PC.string "hgt"
                colon
                hgt <- PT.choice [PT.try $ parseCM,PT.try $ parseInches]
                return $ Map.singleton "hgt" (HGT hgt) 

parseHairColor = do 
                PT.try $ PC.string "hcl"
                colon
                PC.char '#'
                digits <- PT.count 6 PC.hexDigit
                return $ Map.singleton "hcl" (CLR digits)

eyeparse :: (String,EYE) -> PT.ParsecT [Char] st Identity EYE
eyeparse (str,val) = do 
                    PT.try $ PC.string str 
                    return val 

eyes = [("amb",AMB),("blu",BLU),("brn",BRN),("gry",GRY),("grn",GRN),("hzl",HZL),("oth",OTH)]

parseEyeColor = do 
                PT.try $ PC.string "ecl"
                colon
                eyeclr <- PT.choice $ map eyeparse eyes
                return $ Map.singleton "ecl" (ECL eyeclr)

parsePassport = do 
                PT.try $ PC.string "pid"
                colon 
                number <- PT.count 9 PC.digit
                return $ Map.singleton "pid" (PID number)

parseCountry = do 
                PT.try $ PC.string "cid"
                colon 
                PT.many PC.digit
                return $ Map.singleton "cid" (CID)


parseField = do 
            x <- PT.choice [parseBirthYear,parseIssueYear,parseExpirationYear,parseHeight,parseHairColor,parseEyeColor,parsePassport,parseCountry]
            PT.eof
            return x 
                    
 
validatePassport  :: [String] -> Bool
validatePassport fields = let 
                             list = map (\x -> takeWhile (/=':') x) fields
                             len = length list
                            in 
                                if len == 8 then 
                                    True 
                                else 
                                   notElem "cid" list && len == 7 


                                    
validateBetter :: [String] -> IO Bool
validateBetter fields = let 
                            parseres  = map (\x -> PT.parse parseField x x) fields
                            parsesuc = rights parseres

                            total = Map.unions parsesuc
                            keylist = Map.keys total
                            keylength = length keylist 
                        in do 
                            print $ lefts parseres
                            return $ if keylength == 8 then 
                                True 
                            else 
                                keylength == 7  && notElem "cid" keylist 

day4 :: IO ()
day4 = do 
        file <- lines <$> readFile "inputs/inputd4.txt"
        let passports = packPassport file 
        print $ length $ filter (id) $ map validatePassport passports 
        l <- mapM validateBetter passports
        print $ length $ filter (id) l
        return ()

toPart 'F' = 0 
toPart 'B' = 1 
toPart 'R' = 1
toPart 'L' = 0 


toIndex :: String -> Int 
toIndex idx = let 
                mask = zip (reverse $ map toPart idx) (iterate (*2) 1)
            in 
                sum $ map (uncurry (*)) mask 

parseRow = do 
            row <- PT.count 7 $ PT.choice [PT.try$PC.char 'F',PT.try$PC.char 'B']
            col <- PT.count 3 $ PT.choice [PT.try$ PC.char 'L',PT.try$ PC.char 'R']
            return (toIndex row,toIndex col)



day5 :: IO ()
day5 = do 
        file <- lines <$> readFile "inputs/inputd5.txt"
        let res = mapM (PT.parse parseRow "") file
        let ids = fmap (map (\(x,y) -> 8*x + y) ) res 
        print $ fmap maximum ids 
        print $ do 
                    list <- res
                    let (rows, cols) = unzip list 
                    let maxrow = maximum rows 
                    let minrow = minimum rows 
                    let maxcol = maximum cols 
                    let mincol = minimum cols 
                    let trials = do 
                                    r <- [minrow+1..maxrow-1]
                                    c <- [mincol..maxcol]
                                    return (r,c)
                    let (row,col) = head (trials \\ list)
                    return (8*row + col)


toAnyoneCount :: [String] -> Int
toAnyoneCount lst = Set.size $ foldr (Set.union) Set.empty $ map Set.fromList lst 


toEveryoneCount :: [String] -> Int 
toEveryoneCount lst = let 
                        (x:xs) = map Set.fromList lst
                        in 
                        Set.size $ foldr Set.intersection x xs 

day6 = do 
        r <- packPassport <$> lines <$> readFile "inputs/inputd6.txt" 
        print $ sum $ map toAnyoneCount r
        print $ sum $ map toEveryoneCount r 



descriptor = PT.many PC.letter 
color = PT.many PC.letter


bag = do 
        desc <- descriptor
        PC.space 
        col <- color
        PC.space
        PC.string "bag"
        PT.optional $ PC.char 's'
        return (desc,col) 

child :: PT.ParsecT [Char] st Identity (([Char], [Char]),Int)
child = do 
            cnt <- PN.parseIntegral
            PC.space
            bg <- bag
            return (bg,cnt)

eol = do 
        PC.string "no other bags"
        return []

line = do 
        wrapper <- bag
        PC.space
        PC.string "contain"
        PC.space
        chld <- PT.choice [PT.try $ eol,PT.sepBy1 child (PC.string ", ")]
        PT.char '.'
        return (wrapper,Map.fromList chld)


graph = PT.many $ do 
                    l <- line 
                    PC.endOfLine
                    return l


tgtbag :: ([Char], [Char])
tgtbag = ("shiny","gold")


canhold :: Ord k => k -> [(b, Map.Map k a)] -> [b]
canhold tgt lst = do 
                (container,children) <- lst
                if Map.member tgt children then 
                    return container 
                else 
                    []


canholdtree tgt lst current = let 
                                immediate = canhold tgt lst
                                remainder = filter (\x -> not $ Set.member x current) immediate
                                next = Set.union current $ Set.fromList remainder 
                                updates = map (\x -> canholdtree x lst) remainder
                                new = foldr (\ a b ->  a b) next updates
                                in 
                                    if length remainder == 0 then 
                                        next 
                                    else 
                                        new 



treetotal :: (Num a, Ord k) => k -> Map.Map k (Map.Map k a) -> a
treetotal tgt tree = let 
                        children = tree Map.! tgt
                        cnt = sum $ map (\(x,c) -> c * (treetotal x tree)) $ Map.toList children 
                    in             
                        if Map.size children == 0 then 
                            1 
                        else 
                            cnt+1
                            


day7 = do 
        l <- readFile "inputs/inputd7.txt"
        let parsed =  PT.parse graph "" l 
        print $ do
                result <- parsed 
                return $ (Set.size $ canholdtree tgtbag result (Set.singleton tgtbag)) - 1
        print $ do 
                result <- parsed 
                let resmap = (Map.fromList result)
                return $ treetotal tgtbag resmap - 1



data Instruction = ACC Int | JMP Int | NOP Int deriving (Show,Eq)

data Computer = Computer {pc :: Int, program :: Array.Array Int Instruction, acc :: Int} deriving (Show,Eq)

parseAcc = do 
            PC.string "acc"
            PC.space 
            n <- PN.parseIntegral
            return $ ACC n 

parseJmp = do 
            PC.string "jmp"
            PC.space 
            n <- PN.parseIntegral
            return $ JMP n

parseNop = do 
            PC.string "nop"
            PC.space 
            n <- PN.parseIntegral
            return $ NOP n 

parseInstruction = do
                    inst <- PT.choice $ map PT.try [parseAcc,parseJmp,parseNop]
                    PC.endOfLine
                    return inst

parseProgram = do 
                intrs <- PT.many parseInstruction 
                return $ Computer {
                    pc = 0,
                    acc = 0, 
                    program = Array.listArray (0,(length intrs)-1) intrs
                }

liftParse :: (Show a) => Either a b -> IO b 
liftParse (Left a) = fail $ (show a)
liftParse (Right b) = return b


apply :: Instruction -> Computer -> Computer
apply (NOP _) x = x {pc=succ (pc x)}
apply (ACC ac) x = x {pc = succ(pc x), acc = (acc x) + ac}
apply (JMP jm) x = x {pc = (pc x) + jm}

step :: Computer -> Computer
step x = let
            prg = program x
            cnt = pc x 
            instr = prg Array.! cnt 
        in 
            apply instr x 

isHalt :: Computer -> Bool 
isHalt x = let 
            prg = program x 
            cnt = pc x 
            (_,l) = Array.bounds prg
            in 
                cnt == l
 

chkhalts :: Computer -> Set.Set Int -> Either Computer Computer
chkhalts x visited | isHalt x = Right x
chkhalts x visited = let 
                        xp = step x 
                        newvisited = Set.insert (pc x) visited
                     in 
                        if Set.member (pc xp) visited then 
                             Left x 
                        else 
                            chkhalts xp newvisited


cnginstr :: Int -> Computer -> Computer
cnginstr i x = let 
             instr = program x Array.! i
             newinstr = case instr of 
                            ACC n -> ACC n 
                            JMP n -> NOP n 
                            NOP n -> JMP n 
             newprog = (program x) Array.// [(i,newinstr)]
             in 
                 x {program = newprog}



day8 :: IO ()
day8 = do 
        lns <- readFile "inputs/inputd8.txt"
        prg <- liftParse $ PT.parse parseProgram "" lns
        print $ case chkhalts prg Set.empty of 
                    Left x -> acc x 
                    Right x -> acc x 

        let res = map ((`chkhalts` Set.empty).(`cnginstr` prg)) $ Array.indices $ program prg
        let lst = do 
                    x <- res 
                    case x of 
                        Left x -> []
                        Right x -> [x]
        print $ map acc lst



hassum :: [Int] -> Int -> Bool 
hassum [] _ = False 
hassum (x:xs) n |  (n-x) `List.elem` xs = True 
hassum (_:xs) n  = hassum xs n 

checklist :: [Int] -> [Int] -> Int -> (Int,Int)
checklist (x:xs) seen i | hassum seen x = let 
                                            newseen = tail seen ++ [x]
                                            in checklist xs newseen (i+1)

checklist (x:_) _ i = (x,i) 

setsum :: (Eq a, Num a) => [a] -> a -> [[a]]
setsum lst tgt = do 
                    start <- [0..length lst-1]
                    let remainder = length lst - start 
                    remlen <- [2..remainder]
                    let prefix = drop start lst 
                    let rest = take remlen prefix
                    guard (sum rest == tgt)
                    return rest

key :: (Num a, Foldable t, Ord a) => t a -> a
key lst = maximum lst + minimum lst

day9 :: IO ()
day9 = do 
        its <- readInts "inputs/inputd9.txt"
        print $ take 25 its 
        let (num,_) =  checklist (drop 25 its) (take 25 its) 25
        print num 
        print $ map key $ setsum its num 



attempt :: [Integer]
attempt = [16,10,15,5,1,11,7,19,6,12,4]

attempt2 :: [Integer]
attempt2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]

permute 1 = 1 
permute 2 = 2 
permute 3 = 4 
permute 4 = 7

day10 :: IO ()
day10 = do 
            jfile <- readInts "inputs/input10.txt"
            --let jfile = attempt2
            let jolts = List.sort jfile
            let target = maximum jolts + 3
            let jtotal = [0] ++ jolts ++ [target]
            let dist =  zipWith (-) (tail jtotal) jtotal
            let diff3 = length $ filter (==3) dist 
            let diff1 = length $ filter (==1) dist 
            print dist
            print diff1
            print diff3
            print (diff1*diff3)
            let ns = map length $ filter (\x -> head x == 1) $ List.group dist
            print $ product $ map permute ns  

data Seat = Occupied | Empty | Floor deriving (Show,Eq)


readEmpty =  do
                PC.char 'L' 
                return Empty 
readOccupied = do 
                PC.char '#'
                return Occupied 
readFloor = do 
                PC.char '.'
                return Floor

readElement = PT.choice $ map PT.try [readEmpty,readOccupied,readFloor]

readLine = do 
            elements <- PT.many readElement
            PC.endOfLine
            return $ Map.fromList $ zip [0..] elements

readGrid = do 
            lines <- PT.many readLine
            let maps = map (\(l,m) -> Map.mapKeys (\x -> (l,x)) m) $ zip [0..] lines 
            return $ Map.unions maps 

increments :: [(Int,Int)]
increments = [(1,0),(-1,0),(1,-1),(0,-1),(-1,-1),(1,1),(0,1),(-1,1)]

getNeighbours :: Map.Map (Int,Int) Seat -> (Int,Int) -> [(Int,Int)]
getNeighbours grid (x,y) = let 
                                neighbours = map (\(f1,f2) -> (x+f1,y+f2)) increments
                            in 
                                filter (\x -> Map.member x grid) neighbours



getNeighboursLinear:: Map.Map (Int,Int) Seat -> (Int,Int) -> [(Int,Int)]
getNeighboursLinear  grid loc = let 
                                    traceline (x0,y0) (f1,f2) = let 
                                                            loc = (f1+x0,f2+y0)
                                                            potentialPlace = Map.lookup loc grid
                                                        in case potentialPlace of 
                                                            Nothing -> []
                                                            Just x -> case x of 
                                                                        Floor -> traceline loc (f1,f2)
                                                                        Occupied -> [loc]
                                                                        Empty -> [loc]
                                in 
                                    concatMap (traceline loc) increments 
                                        

populateNeighbours :: (Map.Map (Int,Int) Seat -> (Int,Int) -> [(Int,Int)]) -> Map.Map (Int,Int) Seat -> Map.Map (Int,Int) [(Int,Int)]
populateNeighbours f m = Map.mapWithKey (\loc _ -> f m loc) m 

occupiedNeighbours :: Map.Map (Int,Int) Seat -> Map.Map (Int,Int) [(Int,Int)] -> (Int,Int) -> Int 
occupiedNeighbours values neighbours loc = let 
                                            res = do 
                                                    r <- Map.lookup loc neighbours 
                                                    return $ length $ filter (==Occupied) $ map (\x -> Map.findWithDefault Floor x values) r 
                                            in 
                                                case res of 
                                                    Nothing -> 0 
                                                    Just x -> x 


part1Update :: Int -> Seat -> Seat
part1Update _  Floor = Floor 
part1Update n Empty | (n == 0) = Occupied
part1Update _ Empty = Empty 
part1Update n Occupied | (n >= 4) = Empty 
part1Update _ Occupied = Occupied

part2Update :: Int -> Seat -> Seat 
part2Update _  Floor = Floor 
part2Update n Empty | (n == 0) = Occupied
part2Update _ Empty = Empty 
part2Update n Occupied | (n >= 5) = Empty 
part2Update _ Occupied = Occupied

seatStep :: (Int -> Seat -> Seat) -> Map.Map (Int,Int) [(Int,Int)] -> Map.Map (Int,Int) Seat -> Map.Map (Int,Int) Seat 
seatStep update neighbours start = Map.mapWithKey (\ key value -> update (occupiedNeighbours start neighbours key) value) start 

converge :: (Eq a) => (a->a) -> a -> a  
converge step first = let 
                        next = step first
                in 
                    if next == first then 
                        next 
                    else 
                        converge step next 

countOccupied x = let 
                    (_,values) = unzip $ Map.toList x
                in 
                    length $ filter (==Occupied) values

day11 :: IO ()
day11 = do 
            file <- readFile "inputs/inputd11.txt"
            let res = PT.parse readGrid "" file 
            print $ do 
                        r <- res 
                        let simpleneighbours = populateNeighbours getNeighbours r 
                        let simpleconverge = converge (seatStep part1Update simpleneighbours) r
                        let simple =  countOccupied simpleconverge 
                       
                        return simple
            print $ do 
                        r <- res 
                        let compneighbours = populateNeighbours getNeighboursLinear r 
                        let st = Map.foldr Set.insert Set.empty $  Map.map length compneighbours 
                        let compconverge = converge (seatStep part2Update compneighbours) r 
                        let comp = countOccupied compconverge
                        return comp

data Ship = Ship (Int,Int) Int deriving (Show,Eq)

data Waypoint = Waypoint (Int,Int) (Int,Int) deriving (Show,Eq)

data NavInstruction = NORTH | SOUTH | EAST | WEST | FORWARD | LEFT | RIGHT deriving (Show,Eq)

normalize :: (Ord p, Num p) => p -> p
normalize angle | angle >= 360 = normalize (angle-360)
normalize angle | angle < 0 = normalize (angle+360)
normalize angle = angle 

addangle :: (Ord p, Num p) => p -> p -> p
addangle x y = normalize (x+y)


shipmove :: Ship -> (NavInstruction, Int) -> Ship
shipmove (Ship (x,y) a) (NORTH,d) = Ship (x,y+d) a 
shipmove (Ship (x,y) a) (SOUTH,d) = Ship (x,y-d) a 
shipmove (Ship (x,y) a) (EAST,d) = Ship (x+d,y) a 
shipmove (Ship (x,y) a) (WEST,d) = Ship (x-d,y) a 
shipmove (Ship (x,y) a) (LEFT,d) = Ship (x,y) (addangle d a)
shipmove (Ship (x,y) a) (RIGHT,d) = Ship (x,y) (addangle a (-d))
shipmove (Ship (x,y) 0) (FORWARD,d) = Ship (x+d,y) 0 
shipmove (Ship (x,y) 90) (FORWARD,d) = Ship (x,y+d) 90 
shipmove (Ship (x,y) 180) (FORWARD,d) = Ship (x-d,y) 180 
shipmove (Ship (x,y) 270) (FORWARD,d) = Ship (x,y-d) 270


waypointmove :: Waypoint -> (NavInstruction, Int) -> Waypoint
waypointmove (Waypoint (x,y) (r,l)) (NORTH,d) = Waypoint (x,y) (r,l+d)
waypointmove (Waypoint (x,y) (r,l)) (SOUTH,d) = Waypoint (x,y) (r,l-d)
waypointmove (Waypoint (x,y) (r,l)) (EAST,d) = Waypoint (x,y) (r+d,l)
waypointmove (Waypoint (x,y) (r,l)) (WEST,d) = Waypoint (x,y) (r-d,l)
waypointmove (Waypoint (x,y) (r,l)) (LEFT,90) = Waypoint (x,y) (-l,r)
waypointmove (Waypoint (x,y) (r,l)) (LEFT,270) = Waypoint (x,y) (l,-r)
waypointmove (Waypoint (x,y) (r,l)) (LEFT,180) = Waypoint (x,y) (-r,-l)
waypointmove (Waypoint (x,y) (r,l)) (RIGHT,90) = Waypoint (x,y) (l,-r)
waypointmove (Waypoint (x,y) (r,l)) (RIGHT,180) = Waypoint (x,y) (-r,-l)
waypointmove (Waypoint (x,y) (r,l)) (RIGHT,270) = Waypoint (x,y) (-l,r)
waypointmove (Waypoint (x,y) (r,l)) (FORWARD,n) = Waypoint (x+n*r,y+n*l) (r,l)

shipstart :: Ship
shipstart = Ship (0,0) 0

waypointstart :: Waypoint
waypointstart = Waypoint (0,0) (10,1)

options :: [(Char, NavInstruction)]
options = [('N',NORTH),('S',SOUTH),('E',EAST),('W',WEST),('L',LEFT),('R',RIGHT),('F',FORWARD)]

day12parser :: (Integral b, Read b) => (Char, a) -> PT.ParsecT [Char] u Identity (a, b)
day12parser (l,t) = do 
            PC.char l 
            n <- PN.parseIntegral
            PT.endOfLine
            return (t,n)

day12fileparse :: PT.ParsecT [Char] u Identity [(NavInstruction, Int)]
day12fileparse = PT.many $ PT.choice $ map (PT.try.day12parser) options 

testd12 :: [(NavInstruction, Integer)]
testd12 = [(FORWARD,10),(NORTH,3),(FORWARD,7),(RIGHT,90),(FORWARD,11)]

runship :: Foldable t => t (NavInstruction, Int) -> Ship
runship seq = foldl shipmove shipstart seq

shipmanhat :: Ship -> Int
shipmanhat (Ship (x,y) _) = abs x + abs y
 
waymanhat :: Waypoint -> Int
waymanhat (Waypoint (x,y) _) = abs x + abs y 

runwaypoint :: Foldable t => t (NavInstruction, Int) -> Waypoint
runwaypoint seq= foldl waypointmove waypointstart seq 

day12=do 
        f <- readFile "inputs/inputd12.txt"
        let r = PT.parse day12fileparse "" f 
        print $ do 
                    l <- r
                    return (shipmanhat $ runship l,waymanhat $ runwaypoint l)

parseBusBlank = do 
                PC.char 'x'
                return Nothing 

parseBusID = do 
                n<- PN.parseIntegral
                return $ Just n 
parsebus = do 
            time <- PN.parseIntegral
            PC.endOfLine
            lst <- PT.sepBy (PT.choice $ map PT.try [parseBusBlank,parseBusID]) (PC.char ',')
            return (time,lst)


crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a


day13 = do 
        l <- readFile "inputs/inputd13.txt"
        let res = PT.parse parsebus "" l 
        print $ do 
                    (t,lst) <- res
                    let lsttest = mapMaybe id lst 
                    let times = List.sortOn snd $ map (\x -> (x,x - mod t x)) lsttest
                    let (r1,r2) = head times 
                    let keys = mapMaybe (\(i,v)-> fmap (\x -> (x-i,x)) v) $ zip [0..] lst
                    let resultPart2 = crt keys 
                    return (r1*r2,fst resultPart2)  


data InitInstruction = Assignment Int Int | Mask String deriving (Show,Eq)

parseMask = do 
    PC.string "mask"
    PC.space 
    PC.char '='
    PC.space 
    msk <- PT.many $ PT.choice $ map (PT.try.PC.char) ['X','0','1']
    return $ Mask msk

parseAssignment = do 
        PC.string "mem"
        PC.char '['
        to <- PN.parseIntegral
        PC.char ']'
        PC.space 
        PC.char '='
        PC.space 
        Assignment to <$> PN.parseIntegral

parseDay14Line = do 
            inst <-  PT.choice $ map PT.try [parseMask,parseAssignment]
            PT.endOfLine
            return inst

parseDay14File = PT.many parseDay14Line

pwrs2 = map (2 ^) [0..]

parseMaskV1 msk = let 
                    flipmsk = List.reverse msk
                    position = zip flipmsk pwrs2
                    setters = map snd $ filter (\x -> fst x == '1') position
                    clearers = map snd $ filter (\x -> fst x == '0') position
                    in 
                        (sum setters,Bits.complement $ sum clearers)

parseMaskV2 msk = let 
                    flipmsk = List.reverse msk
                    position = zip flipmsk pwrs2
                    setters = map snd $ filter (\x -> fst x == '1') position
                    options = map snd $ filter (\x -> fst x == 'X' ) position
                    in 
                        (sum setters,Bits.complement $ sum options,options)

applyMask :: Int -> Int -> Int -> Int 
applyMask ormask andmask value = (value Bits..&. andmask) Bits..|. ormask  

data Day14Machine = Day14Machine {andmask :: Int, ormask :: Int,alternations::[Int], memory :: Map.Map Int Int}

initMachine = Day14Machine {andmask = Bits.complement 0,ormask = 0,alternations=[], memory = Map.empty}

runMachine machine instruction = case instruction of 
                                     Mask str -> let 
                                                    (set,clr) = parseMaskV1 str 
                                                    in machine { andmask = clr , ormask = set}
                                     Assignment to value ->  machine {memory = Map.insert to (applyMask (ormask machine) (andmask machine) value) $ memory machine}

runMachine2 machine instruction = case instruction of 
                                     Mask str -> let 
                                                    (or,and,alter) = parseMaskV2 str  
                                                in machine {ormask = or, andmask = and, alternations = alter}
                                     Assignment to value ->  machine {memory = setoptions value (applyMask (ormask machine) (andmask machine) to) (alternations machine) (memory machine)}

setoptions::Int -> Int -> [Int] -> Map.Map Int Int -> Map.Map Int Int 
setoptions value address [] memory = Map.insert address value memory 
setoptions value address (x:xs) memory = setoptions value (address+x) xs $ setoptions value address xs memory 

sumMemory machine = sum $ map snd $ Map.toList $ memory machine

day14 :: IO ()
day14 = do 
        f <- readFile "inputs/inputd14.txt"
        let instrs = PT.parse parseDay14File "" f 
        print $ do 
                    l <- instrs 
                    let endstate = foldl runMachine initMachine l
                    let endstate2 = foldl runMachine2 initMachine l
                    return (sumMemory endstate,sumMemory endstate2)
day15input = [9,12,1,4,17,0,18]
day15test= [0,3,6]

advance (previous,lookup,l) = let 
                            newdict = Map.insert previous l lookup
                          in 
                              case Map.lookup previous lookup of 
                                  Nothing -> (0,newdict,l+1)
                                  Just x -> (l-x,newdict,l+1)
                                  



seedday15 lst = let 
                    seq = List.reverse lst 
                in 
                    (head seq,Map.fromList $ zip (init lst) [1..],length seq)

step2nth entry n = grabnth (seedday15 entry) n


grabnth (prev,_,l) n | l == n = prev
grabnth state n = grabnth (advance state) n

day15 = do 
            print $ step2nth day15input 2020
            print $ step2nth day15input 30000000

parseRange = do 
                r1 <- PN.parseIntegral
                PC.char '-'
                r2 <- PN.parseIntegral 
                return (r1,r2)

parseticketField = do 
                key <- PT.many $ PT.choice $ map PT.try [PC.letter,PC.space]
                PC.char ':'
                PC.space 
                r1 <- parseRange 
                PC.string " or "
                r2 <- parseRange
                PT.endOfLine 
                return $ Map.singleton key (r1,r2)

parseConstraints = do 
                    constraints <- PT.many $ PT.try parseticketField
                    PC.endOfLine
                    return $ Map.unions constraints 

parseTicket = do 
                lst <- PT.sepBy PN.parseIntegral (PC.char ',') 
                PT.endOfLine 
                return lst

parseMyTicket = do 
                    PC.string "your ticket:"
                    PC.endOfLine 
                    t<-parseTicket
                    PC.endOfLine 
                    return t 
parseOtherTickets = do  
                    PC.string "nearby tickets:"
                    PC.endOfLine 
                    PT.many parseTicket 


type Range = (Integer,Integer)
type Ticket = [Integer]

parseDay16File :: PT.ParsecT [Char] u Identity (Map.Map String (Range,Range), Ticket,[Ticket])
parseDay16File = do 
                    c<-parseConstraints 
                    mt <- parseMyTicket
                    ot <- parseOtherTickets
                    return (c,mt,ot)

constraint :: Ord r => (r,r) -> r -> Bool 
constraint (r1,r2) r = r <= r2 && r >= r1 

constraints (r1,r2) r = constraint r1 r || constraint r2 r 

filterMap :: Map.Map String (Range,Range) -> Map.Map String (Integer -> Bool)
filterMap dict = Map.map constraints dict 

validValue :: Map.Map String (Integer->Bool) -> Integer -> Bool 
validValue dict = let 
                        (_,cs) = unzip $ Map.toList dict
                    in foldl (\f1 f2 -> \x -> (f1 x) || (f2 x) ) (\x -> False) cs

scanCode :: Map.Map String (Integer->Bool) -> Ticket -> Integer
scanCode filter ticket = sum $ List.filter (\x -> not $ validValue filter x) ticket

solvedday16Part1 :: (Map.Map String (Range, Range), Ticket, [Ticket]) -> Integer
solvedday16Part1 (c,m,nt) = sum $ map (scanCode (filterMap c)) nt


keys = ["departure location","departure station","departure platform","departure track","departure date","departure time"]

fieldOptions :: Map.Map String (Integer -> Bool) -> [Integer] -> [String]
fieldOptions filterset lst = let
                            adjust x = foldl (\r y -> r && x y) True lst
                            in map fst $ List.filter (\(x,y) -> adjust y) $ Map.toList filterset

solved :: [[String]] ->  Map.Map String Integer -> (Map.Map String Integer,[[String]])
solved [] a = (a,[]) 
solved lst a = let 
                    lengths = map length lst
                    pos = List.elemIndex 1 lengths
                in 
                    case pos of 
                        Nothing -> (a,lst)
                        Just i -> let 
                                    fix = lst !! i
                                    key = head fix 
                                    rem = map (List.delete key) lst
                                    updated = Map.insert key (toInteger i) a 
                                    in 
                                        solved rem updated

solvedday16Part2 (c,m,nt) = let 
                                filters = filterMap c 
                                remtickets = List.filter (\x -> 0 == scanCode filters x) nt 
                                cols = List.transpose remtickets
                                options = map (fieldOptions filters) cols 
                                (orders,remainder) = solved options Map.empty
                                solveset = Map.fromList $ zip  keys [0..]
                                solutionkeys = Map.intersection orders solveset
                                (_,ticketlocs) = unzip $ Map.toList solutionkeys
                            in  product $ map (\x -> m !! (fromIntegral x)) ticketlocs 

day16 = do 
            f <- readFile "inputs/inputd16.txt"
            let r = PT.parse parseDay16File "" f
            print $ do 
                        l <- r 
                        return $ (solvedday16Part1 l,solvedday16Part2 l)

type PocketLocation = (Integer,Integer,Integer)
type PocketDimension = Set.Set PocketLocation 

parseCellDay17 = do 
                    cell <- PT.choice $ map PT.try [PC.char '.',PC.char '#']
                    return (cell == '#')

parseLineDay17 = do 
                    manyCells <- PT.many parseCellDay17
                    PC.endOfLine
                    return  $ map snd (filter fst $ zip manyCells ([0..]::[Integer]))

parseDay17 :: PT.ParsecT String u Identity (Set.Set (Integer, Integer, Integer))
parseDay17 = do 
                actives <- PT.many parseLineDay17
                return $ Set.fromList $ do 
                                            (row,i) <- zip actives ([0..]::[Integer])
                                            map (\x -> (0,i,x)) row 
               
getNeighbours3d :: PocketLocation -> [PocketLocation]
getNeighbours3d (x,y,z) = do 
                            x' <- [-1,0,1]
                            y' <- [-1,0,1]
                            z' <- [-1,0,1]
                            guard ((x',y',z') /= (0,0,0))
                            return (x+x',y+y',z+z')

getNeighbours4d (x,y,z,w) = do 
                            x' <- [-1,0,1]
                            y' <- [-1,0,1]
                            z' <- [-1,0,1]
                            w' <- [-1,0,1]
                            guard ((x',y',z',w') /= (0,0,0,0))
                            return (x+x',y+y',z+z',w+w')

day17rule n True | n==2 || n==3 = True 
day17rule n True = False
day17rule n False | n == 3 = True 
day17rule n state = state 


day17Step :: Ord a => (a->[a]) -> Set.Set a -> Set.Set a 
day17Step neighbours state = let 
                    currentActive = Set.toList state
                    potentialActive = currentActive >>= neighbours  
                    res = do 
                            candidate <- potentialActive
                            let currentState = Set.member candidate state
                            let numNeighbours = length $ filter (\x-> Set.member x state) $ neighbours candidate
                            if day17rule numNeighbours currentState then 
                                return candidate 
                            else 
                                []
                in Set.fromList res 


teststate = [(0,0,1),(0,1,2),(0,2,0),(0,2,1),(0,2,2)]

to4d (x,y,z) = (0,x,y,z)

toHyper state = Set.fromList $ map to4d $ Set.toList state 

day17 = do 
            f <- readFile "inputs/inputd17.txt"
            let r = PT.parse parseDay17 "" f 
            
            print $ do 
                        l <- r
                        let progress3d = iterate (day17Step getNeighbours3d) l
                        let result x = head $ List.drop 6 $ map Set.size x
                        let progress4d = iterate (day17Step getNeighbours4d) $ toHyper l 
                        return (result progress3d,result progress4d)


data Op = Add | Mul deriving (Show,Eq)
data Expression  = Value Integer | Bracket Expression | Operations [Op] [Expression] deriving (Show,Eq)


compute Add x y = x + y 
compute Mul x y = x * y

lhprecedence (Value a) = a 
lhprecedence (Bracket e) = lhprecedence e 
lhprecedence (Operations [] (x:[])) = lhprecedence x 
lhprecedence (Operations (x:rest) (a:b:restval)) = let 
                                                    left = lhprecedence a 
                                                    right = lhprecedence b 
                                                    new = compute x left right 
                                                    in 
                                                        lhprecedence (Operations rest ((Value new):restval))


reduceAdd (Value a) = Value a 
reduceAdd (Bracket e) = Bracket (reduceAdd e)
reduceAdd (Operations (Add:[]) vals) = Operations (Add:[]) $ map reduceAdd vals
reduceAdd (Operations (Mul:[]) vals) = Operations (Mul:[]) $ map reduceAdd vals 
reduceAdd (Operations ops values) = let
                                        reduced = map reduceAdd values
                                        addloc = List.elemIndex Add ops 
                                    in 
                                        case addloc of
                                            Nothing -> (Operations ops reduced) -- There are no add operations
                                            Just loc -> let
                                                            (opl,opr) = List.splitAt loc ops
                                                            (op:oprest) = opr 
                                                            (vall,l:r:valrrest) = List.splitAt loc reduced
                                                        in 
                                                            reduceAdd $ Operations (opl++oprest) (vall++[Operations [Add] (l:r:[])]++valrrest)



tests = [
        "1 + 2 * 3 + 4 * 5 + 6",
        "1 + (2 * 3) + (4 * (5 + 6))",
        "2 * 3 + (4 * 5)",
        "5 + (8 * 3 + 9 + 3 * 4 * 3)",
        "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
        "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"]

testresults=[71,51,26,437,12240,13632]

parseMultiply = PT.string " * " >>= (\_ -> return Mul)
parseAdd = PT.string " + " >>=(\_ -> return Add)
parseOperator = PT.choice [PT.try parseMultiply, PT.try parseAdd]

parseValue = fmap Value PN.parseIntegral
parseBracketedExpression = fmap Bracket $ PT.between (PC.char '(') (PC.char ')') parseExpression
parseEquationValue = PT.choice [PT.try parseBracketedExpression,parseValue]

parseExpression = do 
                    eq <- parseEquationValue
                    opm <- PT.optionMaybe parseOperator 
                    case opm of 
                        Nothing -> return eq 
                        Just op -> do 
                                    rest <- parseExpression
                                    return $ case rest of 
                                                Value x -> Operations [op] [eq,Value x]
                                                Operations ops eqs -> Operations (op:ops) (eq:eqs)
                                                Bracket exp -> Operations [op] (eq:Bracket exp:[])



day18 = do 
            f <- lines <$> readFile "inputs/inputd18.txt"
            let values = map (PT.parse parseExpression "") f
            let ([],results) = partitionEithers values
            print $ sum $ map lhprecedence results 
            print $ sum $ map (lhprecedence.reduceAdd) results


data Rule = Fixed String | SubRules [[Integer]] deriving (Show,Eq)


failingParser n = do fail ("could not lookup rule "++(show n))


parseRuleList = PT.sepEndBy1 PN.parseIntegral (PC.char ' ')

parseBranchRule = do 
                    l <- PT.sepBy1 parseRuleList (PC.string "| ")
                    PT.endOfLine
                    return $ SubRules l

parseStaticRule = do 
                list <- PT.between (PT.char '\"') (PT.char '\"') (PT.many1 PC.letter)
                PC.endOfLine
                return (Fixed list)

parseRule = do 
            rulenumber <- PN.parseIntegral
            PC.char ':'
            PC.char ' ' 
            rule <- PT.choice [parseStaticRule,parseBranchRule]
            return (rulenumber,rule)

parseRuleTable = fmap Map.fromList $ PT.many parseRule 

parseString = do 
                l <- PT.many PC.letter 
                PT.endOfLine
                return l 

parseFullFile = do 
                rules <- parseRuleTable 
                PT.endOfLine
                lines <- PT.many parseString 
                return (rules,lines)

constructAndList table (xs) = let 
                                andlist = map (\x -> fromMaybe (failingParser x) $ fmap (constructSubParser table) $ Map.lookup x table) xs 
                                in fmap (concat) $ sequence andlist 

constructSubParser :: Map.Map Integer Rule -> Rule -> PT.ParsecT String a Identity String 
constructSubParser table (Fixed str) = PC.string str
constructSubParser table (SubRules xs) = PT.choice $ map (PT.try . constructAndList table) xs


day19 = do 
            f <- readFile "inputs/inputd19.txt"
            let parsed = PT.parse parseFullFile "" f 
            (parsetree,lines) <- case parsed of 
                        Left x -> fail (show x) 
                        Right x -> return x 
            parser <- do 
                        value <- case Map.lookup 0 parsetree of
                                        Nothing -> fail "could not find 0 parser"
                                        Just x -> return x 
                        return $ do 
                            p <- constructSubParser parsetree value 
                            PT.eof
                            return p

            let (lefts,rights) = partitionEithers $ map (PT.parse parser "") lines 
            print $ length lefts 
            print $ length rights 