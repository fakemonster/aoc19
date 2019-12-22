import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import qualified Data.Text as T
import System.Environment

main = part2

part1 = head <$> getArgs >>=
  readFile >>=
  print . checkSum 0 "COM" . toOrbitMap . readOrbits

part2 = head <$> getArgs >>=
  readFile >>=
  print . closestPath "YOU" "SAN" . toOrbiterMap . readOrbits

type Depth = Integer
type Name = String
type Orbit = (String, String)
type Galaxy = M.Map String PSum

data Planet = Moons Name Depth [Planet] | Lonely

{-
 - The issue with the above is that we can't necesarily _order_ inputs! We'll
 - have to do a whole tree search every time...
 -
 - The other option would be to have a Map of the planets, losing the relations
 - in favor of just keeping the depths
 - -}

data PSum = PSum Name Depth

testCase = M.fromList
  [ ("B",["G","C"])
  , ("C",["D"])
  , ("COM",["B"])
  , ("D",["I","E"])
  , ("E",["J","F"])
  , ("G",["H"])
  , ("J",["K"])
  , ("K",["L"])
  ]

testCase2 = M.fromList [("B","COM"),("C","B"),("D","C"),("E","D"),("F","E"),("G","B"),("H","G"),("I","D"),("J","E"),("K","J"),("L","K")]

initMap :: Galaxy
initMap = M.fromList
  [ ("COM", PSum "COM" 0)
  ]

toTuple :: [a] -> (a,a)
toTuple (a:b:_) = (a,b)

readOrbits :: String -> [Orbit]
readOrbits = toOrbit . toList
  where toList = fmap T.strip . T.splitOn (T.pack "\n") . T.strip . T.pack
        toOrbit = fmap (toTuple . fmap T.unpack . T.splitOn (T.pack ")"))

orEmpty :: Ord k => k -> M.Map k [a] -> [a]
orEmpty k m = fromMaybe [] $ M.lookup k m

toOrbitMap :: [Orbit] -> M.Map String [String]
toOrbitMap os = foldl extend (M.fromList []) os
  where extend m (a,b) = M.insert a (b:orEmpty a m) m

toOrbiterMap :: [Orbit] -> M.Map String String
toOrbiterMap = foldr (uncurry . flip $ M.insert) (M.fromList [])

path :: String -> M.Map String String -> String -> Maybe [String]
path target m s
  | s == target = Just [target]
  | otherwise = (++[s]) <$> (M.lookup s m >>= path target m)

checkSum :: Depth -> String -> M.Map String [String] -> Integer
checkSum d p m = d + foldl childDepths 0 (orEmpty p m)
  where childDepths n p' = n + checkSum (d+1) p' m

closestPath :: String -> String -> M.Map String String -> Int
closestPath p1 p2 m = addLengths . unzipLong . trimCommon $ zipLong path1 path2
  where path1 = fromMaybe [] $ path "COM" m p1
        path2 = fromMaybe [] $ path "COM" m p2
        trimCommon = dropWhile (uncurry (==))
        addLengths (a,b) = length a + length b - 2

zipLong :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipLong [] [] = []
zipLong (a:as) [] = (Just a, Nothing) : zipLong as []
zipLong [] (b:bs) = (Nothing, Just b) : zipLong [] bs
zipLong (a:as) (b:bs) = (Just a, Just b) : zipLong as bs

with :: (a,b) -> ([a],[b]) -> ([a],[b])
with (a,b) (as, bs) = (a:as, b:bs)

withA :: a -> ([a],[b]) -> ([a],[b])
withA a (as,bs) = (a:as, bs)

withB :: b -> ([a],[b]) -> ([a],[b])
withB b = fmap (b:)

unzipLong :: [(Maybe a, Maybe b)] -> ([a],[b])
unzipLong [] = ([],[])
unzipLong ((Just a, Nothing):rest) = withA a (unzipLong rest)
unzipLong ((Nothing, Just b):rest) = withB b (unzipLong rest)
unzipLong ((Just a, Just b):rest) = (a,b) `with` (unzipLong rest)
