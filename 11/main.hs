{-
 - TODO: Find out why the hell my bot keeps running after it should stop. It
 - managed to spell its output totally correctly! But then it just kept going??
 - And in part 1 it was 60 over the correct answer (I only found the correct
 - answer the first time because I hadn't totally implemented day 9...)
 -}
{-#
 OPTIONS_GHC
 -Wall
 -Wno-unused-binds
 -Wno-unused-imports
 #-}

import VM
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M
-- import Data.List.Split (chunksOf) -- hmmmm....

-- MAIN

main :: IO ()
main = part1

part1 :: IO ()
part1 = do
  vm' <- fromArg 0 0
  let initial = trackedRobot vm'
      final = until (oneOf paintDone (halted . vm . robot1)) iterate' initial
      -- final = iterate' $ initial
  -- print initial
  -- print $ (vm . robot1) final
  writeFile "result.txt" $ customHull final
  -- putStrLn $ (customHull) final
  print $ (M.size . hull1) final

temp :: IO ()
temp = putStrLn "heY"


-- TYPES

data Space = Space
  { hull :: Hull
  , robot :: Robot
  } deriving (Show)

data Space1 = Space1
  { hull1 :: M.Map Pos Panel
  , robot1 :: Robot
  } deriving (Show)

xBounds :: [Pos] -> (Int, Int)
xBounds ps = (minimum xs, maximum xs)
  where xs = fst <$> ps

yBounds :: [Pos] -> (Int, Int)
yBounds ps = (minimum ys, maximum ys)
  where ys = snd <$> ps

intoGrid :: Robot -> M.Map Pos Panel -> (Int, String)
intoGrid r m = (w, grid)
  where (x1, x2) = xBounds (M.keys m)
        (y1, y2) = yBounds (M.keys m)
        offset = (x1, y1)
        w = x2 - x1 + 1
        h = y2 - y1 + 1
        translate = bump offset . trans (w, h)
        range = [0..(w * h) - 1]
        mapOrBot p = if p == (pos r) then (show . dir) r else lookupper p
        lookupper = show . fromMaybe Black . flip M.lookup m
        grid = range >>= (mapOrBot . translate)

customHull :: Space1 -> String
customHull (Space1 m r) = unlines (chunksOf w grid)
  where (w, grid) = intoGrid r m

paintDone :: Space1 -> Bool
paintDone = halted . vm . robot1


data Hull = Hull Width [Panel] deriving (Eq)
type Width = Int
data Robot = Robot
  { dir :: Dir
  , pos :: Pos
  , vm :: VM
  }
type Pos = (Int, Int)
data Dir = U | R | D | L deriving (Eq, Enum, Bounded)
data Panel = Black | White deriving (Eq)

instance Show Hull where
  show (Hull w ps) = unlines $ chunksOf w (ps >>= show)

instance Show Dir where
  show U = "^"
  show R = ">"
  show D = "v"
  show L = "<"

right :: Dir -> Dir
right d
  | d == maxBound = minBound
  | otherwise = succ d

left :: Dir -> Dir
left d
  | d == minBound = maxBound
  | otherwise = pred d

instance Show Panel where
  show Black = "."
  show White = "#"

toPanel :: Int -> Panel
toPanel 1 = White
toPanel _ = Black

toInt :: Panel -> Int
toInt Black = 0
toInt White = 1

instance Show Robot where
  show (Robot d _ _) = show d

newRobot :: VM -> Robot
newRobot = Robot U (0,0)

move :: Robot -> Robot
move (Robot d p v) = Robot d (shift d p) v

turn :: Int -> Robot -> Robot
turn 0 (Robot d p v) = Robot (left d) p v
turn 1 (Robot d p v) = Robot (right d) p v
turn _ (Robot d p v) = traceShow "bad turn!" $ Robot d p v


-- PART 1

trackedRobot :: VM -> Space1
trackedRobot vm' = Space1 (M.fromList [((0,0), White)]) (newRobot (pause vm'))

turn' :: Robot -> Robot
turn' r@(Robot _ _ vm') = turn (readOut vm') r

unpauseBot :: Int -> Robot -> Robot
unpauseBot n (Robot d p vm') = Robot d p (unpause vm' n)

unpauseBot' :: Robot -> Robot
unpauseBot' (Robot d p vm') = Robot d p (unpause' vm')

runBot :: Robot -> Robot
runBot (Robot d p vm') = Robot d p (run vm')

paint :: Robot -> Robot
paint = id

iterate' :: Space1 -> Space1
iterate' (Space1 h r) = Space1 (update h) $ moved
  where n = toInt . fromMaybe Black . M.lookup (pos r) $ h
        unpaused = unpauseBot n r
        painted = unpauseBot' . paint . runBot $ unpaused
        thisPos = pos painted
        panel = toPanel . readOut . vm $ painted
        moved = move . turn' . runBot $ painted
        nextPos = pos moved
        update = M.insert thisPos panel


-- UTIL

oneOf :: (a-> Bool) -> (a -> Bool) -> a -> Bool
oneOf f g a = f a || g a

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n >= length xs = [xs]
  | otherwise = uncurry (:) . fmap (chunksOf n) $ splitAt n xs

demap :: (Int -> Int) -> Pos -> Pos
demap f (x, y) = (f x, y)

shift :: Dir -> Pos -> Pos
shift U = fmap (subtract 1)
shift R = demap (+1)
shift D = fmap (+1)
shift L = demap (subtract 1)

touch :: Ord k => k -> a -> M.Map k a -> M.Map k a
touch = M.insertWith (flip const)

bump :: Pos -> Pos -> Pos
bump (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

trans :: Pos -> Int -> Pos
trans (x, y) i = (i `rem` x, i `div` x)


-- DEBUG

sample :: M.Map Pos Panel
sample = M.fromList
  [ ((0,0), Black)
  ]

aMap :: M.Map Pos Panel
aMap = M.fromList
  [ ((-1,0),Black)
  , ((-1,1),White)
  , ((0,0),Black)
  , ((0,1),White)
  , ((1,-1),White)
  , ((1,0),White)
  , ((2,-1),White)
  ]

whyBad :: Space1
whyBad = Space1 aMap (newRobot blankVM)
