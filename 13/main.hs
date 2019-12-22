{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-unused-imports #-}

import VM
import Debug.Trace
import qualified Data.Map as M

main :: IO ()
main = do
  (output, vm) <- runFullInput
  let screen = render output
  print screen
  -- print $ collect output

-- render :: IO ()

-- TYPES

data Screen = Screen
  { _score :: Int
  , _width :: Int
  , _pixels :: [Pixel]
  }

data Pixel = Empty | Wall | Block | Paddle | Ball

instance Show Pixel where
  show Empty = "."
  show Wall = "#"
  show Block = "■"
  show Paddle = "="
  show Ball = "o"

toPix :: Int -> Pixel
toPix 1 = Wall
toPix 2 = Block
toPix 3 = Paddle
toPix 4 = Ball
toPix _ = Empty

showAll :: Int -> [Pixel] -> String
showAll w px = unlines . chunksOf w $ px >>= show

instance Show Screen where
  show (Screen score width px) = score' ++ "\n" ++ width' ++ "\n" ++ pix
    where score' = show score
          width' = '(' : show width ++ ")"
          pix = showAll width px

type Pos = (Int, Int)
type PixMap = M.Map Pos Pixel

-- PARSE

collect' :: PixMap -> [Int] -> PixMap
collect' ps xs
  | length xs < 3 = ps
  | otherwise = collect' (M.insert (x,y) (toPix z) ps) rest
    where (x:y:z:rest) = xs

collect :: [Int] -> PixMap
collect = collect' $ M.fromList []

toScreen :: PixMap -> Screen
toScreen ps = Screen 0 width pixels
  where width = (+1) . fst . maximum . M.keys $ ps
        pixels = snd <$> M.toList (M.mapKeys swap ps)
        swap (x, y) = (y, x)

render :: [Int] -> Screen
render = toScreen . collect

-- UTIL
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

every :: Eq a => Int -> [a] -> [a]
every n xs
  | rem' == [] = rem'
  | otherwise = head rem' : every n (tail rem')
    where rem' = drop (n - 1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n >= length xs = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)
