import Data.Map (fromList)
import Debug.Trace
import System.Environment

import VM

main = part2

test = do
  text <- head <$> getArgs >>= readFile
  let input = parseState text
      vm = initVM 4 0 input
  print $ run vm
  putStrLn "tested"

part1 = do
  initial <- primeVM
  print $ maximum $ fmap (runOptions initial) options
  print "done"

part2 = do
  initial <- primeVM
  let machines = fmap initial <$> options'
  print $ maximum $ fmap (flip(runOptions') 0) machines
  putStrLn "done"

perms :: [Int] -> [[Int]]
perms [] = []
perms [n] = [[n]]
perms ns = ns >>= concatRecurse
  where concatRecurse n = (n:) <$> perms (filter (/=n) ns)

options :: [[Int]]
options = perms [0..4]

options' :: [[Int]]
options' = perms [5..9]

trackMap :: Functor f => (a -> b) -> f a -> f (a,b)
trackMap = (<$>) . track
-- trackMap f x = track f <$> x

track :: (a -> b) -> a -> (a, b)
track = (>>= flip (,))
-- track f x = (x, f x)

test1 :: (Int -> Int -> VM)
test1 a b = initVM a b [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,4,0]

test2 :: (Int -> Int -> VM)
test2 a b = initVM a b [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

runOptions :: (Int -> Int -> VM) -> [Int] -> Int
runOptions primed xs = foldl (\out phase -> readOut . run $ primed phase out) 0 xs

runOptions' :: [(Int -> VM)] -> Int -> Int
runOptions' machines n = if halted final then readOut final else runOptions' restarted (readOut final)
    where blank = blankWithOutput n
          runFrom ms m = run $ m (readOut . head $ ms)
          nextCycle = tail . reverse $ foldl (\ms m -> (runFrom ms m):ms) [blank] machines
          final = last nextCycle
          restarted = unpause <$> nextCycle

