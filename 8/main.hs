{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Text (pack, strip, unpack)
import System.Environment

main :: IO ()
main = do
  part1
  part2

readInput :: IO (Int, Int, [Layer])
readInput = do
  (file:w:h:_) <- getArgs
  raw <- unpack . strip . pack <$> readFile file
  let width = read w
      height = read h
      layers = splitToLayers width height raw
  return (width, height, layers)

part1 :: IO ()
part1 = do
  (_, _, layers) <- readInput
  let best = fewest '0' layers
  print $ oneByTwo <$> best

part2 :: IO ()
part2 = do
  (width, _, layers) <- readInput
  let rendered = render layers
      output = toTerminal width $ rendered
  putStrLn $ output


type Width = Int
type Height = Int
type Layer = String

data Color = Black | White | Transparent deriving (Eq)

instance Show Color where
  show Black = "."
  show White = "#"
  show Transparent = " "

toChar :: Color -> Char
toChar Black = '.'
toChar White = '#'
toChar Transparent = ' '

splitToLayers :: Width -> Height -> String -> [Layer]
splitToLayers w h s = chunks (w * h) s

fewest :: Char -> [Layer] -> Maybe Layer
fewest c = maximumBy (length . filter (/=c))

maximumBy :: Ord b => (a -> b) -> [a] -> Maybe a
maximumBy _ [] = Nothing
maximumBy _ (x:[]) = Just x
maximumBy f (x:xs) = Just $ foldl (\a b -> if f a > f b then a else b) x xs

oneByTwo :: String -> Int
oneByTwo s = count '1' s * count '2' s
  where count x = length . filter (==x)

toColor :: Char -> Color
toColor '0' = Black
toColor '1' = White
toColor _ = Transparent

mergeColor :: Color -> Color -> Color
mergeColor c1 c2
  | c1 == Transparent = c2
  | otherwise = c1

mergeLayer :: [Color] -> [Color] -> [Color]
mergeLayer = zipWith mergeColor

render :: [Layer] -> [Color]
render ls = foldl mergeLayer blank (fmap toColor <$> ls)
  where blank = replicate (length $ ls !! 0) Transparent

chunks :: Int -> [a] -> [[a]]
chunks l xs
  | length xs == 0 = []
  | length xs < l = [xs]
  | otherwise = c : chunks l rest
      where (c, rest) = splitAt l xs

toTerminal :: Width -> [Color] -> String
toTerminal w cs = unlines $ fmap toChar <$> chunks w cs
