import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import System.Environment
import Text.Read

data Dir = U | R | D | L deriving (Show, Eq)
data Direction = Direction Dir Integer deriving (Show)
type Pos = (Integer, Integer)
type Line = (Pos, Pos)
type Range = (Integer, Integer)
data Cline = Cline Integer Line

instance Show Cline where
  show (Cline n l) = "|" ++ show n ++ "|" ++ showLine l

cLift :: (Line -> a) -> Cline -> a
cLift f = \(Cline _ l) -> f l

cLift2 :: (Line -> Line -> a) -> Cline -> Cline -> a
cLift2 f = \(Cline _ l1) (Cline _ l2) -> f l1 l2

len :: Line -> Integer
len l@((x,y),(x1,y1))
  | isHoriz l = abs (x - x1)
  | otherwise = abs (y - y1)

withCounts :: Integer -> [Line] -> [Cline]
withCounts _ [] = []
withCounts n (l:ls) = Cline n l : withCounts (n + len l) ls

getLines :: String -> [Line]
getLines = toLines (0,0) . readDirections

sample1 = getLines "R8,U5,L5,D3"
sample2 = getLines "U7,R6,D4,L4"

showLine :: Line -> String
showLine ((a,b),(a1,b1)) = "(" ++
                           s a ++ "," ++ s b ++
                           "->" ++
                           s a1 ++ "," ++ s b1 ++
                           ")"
    where s = show

combos :: [a] -> [a] -> [(a,a)]
combos = liftA2 (,)

toLines :: Pos -> [Direction] -> [Line]
toLines _ [] = []
toLines pos (dir:ds) = (pos, next) : toLines next ds
  where next = shift pos dir

shift :: Pos -> Direction -> Pos
shift (x, y) (Direction d n)
  | d == U = (x, y - n)
  | d == R = (x + n, y)
  | d == D = (x, y + n)
  | d == L = (x - n, y)

unpackMaybes :: [Maybe a] -> [a]
unpackMaybes [] = []
unpackMaybes (Nothing:xs) = unpackMaybes xs
unpackMaybes (Just x:xs) = x : unpackMaybes xs

split :: Char -> String -> [String]
split c cs = T.unpack <$> T.splitOn (T.pack [c]) (T.pack cs)

isIntersection :: Line -> Line -> Bool
isIntersection l1@((a,b),_) l2@((x,y),_)
  | isVert l1 && isHoriz l2 = crosses a (xOf l2) && crosses y (yOf l1)
  | isHoriz l1 && isVert l2 = isIntersection l2 l1 -- lol
  | isVert l1 = a == x && overlaps (yOf l1) (yOf l2) -- both vert
  | otherwise = b == y && overlaps (xOf l1) (xOf l2) -- both horiz

cIsIntersection :: Cline -> Cline -> Bool
cIsIntersection = cLift2 isIntersection

xOf :: Line -> Range
xOf ((x,_),(x1,_)) = (x, x1)

yOf :: Line -> Range
yOf ((_,y),(_,y1)) = (y, y1)

isVert :: Line -> Bool
isVert ((a,_),(a1,_)) = a == a1

isHoriz = not . isVert

overlaps :: Range -> Range -> Bool
overlaps (a, a1) r = crosses a r || crosses a1 r

crosses :: Integer -> Range -> Bool
crosses n (x,y)
  | n == x = True
  | n == y = True
  | otherwise = (n > x) /= (n > y)

intersection :: Line -> Line -> Pos
intersection l1@((a,b),_) l2@((x,y),_)
  | isVert l1 && isHoriz l2 = (a,y)
  | isVert l2 && isHoriz l1 = (x,b)
  | otherwise = (x,b)

cIntersection :: Cline -> Cline -> Integer
cIntersection (Cline n1 l1) (Cline n2 l2) =
  n1 + n2 + len (fst l1, point) + len (fst l2, point)
    where point = intersection l1 l2

intersections :: [Line] -> [Line] -> [(Line, Line)]
intersections a b = filter (uncurry isIntersection) $ combos a b

cIntersections :: [Cline] -> [Cline] -> [(Cline, Cline)]
cIntersections a b = filter (uncurry cIsIntersection) $ combos a b

parseDirection :: String -> Maybe Direction
parseDirection "" = Nothing
parseDirection (dir:dist)
  | dir == 'U' = Direction U <$> readMaybe dist
  | dir == 'R' = Direction R <$> readMaybe dist
  | dir == 'D' = Direction D <$> readMaybe dist
  | dir == 'L' = Direction L <$> readMaybe dist
  | otherwise = Nothing

readDirections :: String -> [Direction]
readDirections = unpackMaybes . fmap parseDirection . split ','

manhattan :: Pos -> Integer
manhattan (a,b) = abs a + abs b

main = do
  (filename:_) <- getArgs
  input <- readFile filename

  let lines = getLines <$> filter ((>1) . length ) (split '\n' input)
  -- print $ fmap showLine <$> lines
  let cLines = withCounts 0 <$> lines

  let colls = intersections (lines !! 0 ) (lines !! 1)
  let cColls = cIntersections (cLines !! 0 ) (cLines !! 1)
  -- print cColls

  let ints = uncurry intersection <$> colls
  let cInts = uncurry cIntersection <$> cColls
  -- print cInts

  let dists = manhattan <$> ints
  -- print dists

  let optimal = foldr min 99999999999999 $ manhattan <$> tail ints
  let cOptimal = foldr min 99999999999999 $ tail cInts
  print optimal
  print cOptimal

  -- putStrLn "done"
