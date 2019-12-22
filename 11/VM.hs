module VM
( execute
, run
, initVM
, fromArg
, parseState
, primeVM
, blankVM
, VM
, Rel
, Status
, readOut
, readIn
, runInput
, runFullInput
, runFull
, pause
, unpause'
, unpause
, blankWithOutput
, halted
) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.Environment

runInput :: IO ()
runInput = do
  (file:start:_) <- getArgs
  input <- readFile file
  let vm = initVM 1 (read start) . parseState $ input
  print $ run vm

runFullInput :: IO ()
runFullInput = do
  (file:start:_) <- getArgs
  input <- readFile file
  let vm = initVM 1 (read start) . parseState $ input
      (outputs, vm') = runFull ([], vm)
  print vm'
  print outputs

fromArg :: Int -> Int -> IO VM
fromArg p n = initVM p n . parseState <$> (head <$> getArgs >>= readFile)

primeVM :: IO (Int -> Int -> VM)
primeVM = (\a b c -> initVM b c a) . parseState <$> (head <$> getArgs >>= readFile)

parseState :: String -> [Int]
parseState = fmap (read . T.unpack) . T.splitOn (T.pack ",") . T.pack

initVM :: Int -> Int -> [Int] -> VM
initVM phase input state = VM state 0 (Rel 0) 0 (Status Active Nothing input 0)

blankWithOutput :: Int -> VM
blankWithOutput n = VM [] 0 (Rel 0) 0 (Status Halted Nothing 0 n)

halted :: VM -> Bool
halted = (== Halted) . runState . status

data Mode = Position | Immediate | Relative deriving (Show)
type Parameters = (Mode, Mode, Mode)
data RunState = Active | Paused | Halted deriving (Eq, Show)
data Status = Status
  { runState :: RunState
  , phase :: Maybe Int
  , input :: Int
  , output :: Int
  } deriving (Show)
data VM = VM
  { state :: [Int]
  , ip :: Int
  , rel :: Rel
  , ticks :: Int
  , status :: Status
  } deriving (Show)

data Rel = Rel Int

instance Show Rel where
  show (Rel x) = show x

data Opcode = Opcode Code Length Action
type Code = Int
type Length = Int
type Action = VM -> [Int] -> Parameters -> VM

readOut :: VM -> Int
readOut = output . status

readIn :: VM -> Int
readIn = input . status

opcodes :: M.Map Int Opcode
opcodes = M.fromList
  [ (1,  Opcode 1 3 add)
  , (2,  Opcode 2 3 mult)
  , (3,  Opcode 3 1 store) -- input
  , (4,  Opcode 4 1 (restore . pause)) -- output
  , (5,  Opcode 5 2 jumpIfTrue)
  , (6,  Opcode 6 2 jumpIfFalse)
  , (7,  Opcode 7 3 lt)
  , (8,  Opcode 8 3 eq)
  , (9,  Opcode 9 1 reRel)
  , (99, Opcode 99 0 halt)
  , (-1, bailCode)
  ]

bailCode :: Opcode
bailCode = Opcode (-1) 0 bail

from :: [Int] -> Rel -> Int -> Mode -> Int
from s _ i       Position = safeAt 0 i s
from _ _ i       Immediate = i
from s (Rel x) i Relative = safeAt 0 (x + i) s

to :: Rel -> Mode -> Int -> Int
to _ Position = id
to (Rel x) Relative = (+x)

safeAt :: a -> Int -> [a] -> a
safeAt x i xs
  | i >= 0 && i < length xs = xs !! i
  | otherwise = x

add :: Action
add vm@(VM state _ rel _ _) [a,b,i] (p1, p2, p3) = setState newState vm
  where (left, right) = splitAt' (to rel p3) i state
        one = from state rel a p1
        two = from state rel b p2
        newState = left ++ (one + two) : tail right

mult :: Action
mult vm@(VM state _ rel _ _) [a,b,i] (p1, p2, p3) = setState newState vm
  where (left, right) = splitAt' (to rel p3) i state
        one = from state rel a p1
        two = from state rel b p2
        newState = left ++ (one * two) : tail right

store :: Action
store vm@(VM state _ rel _ (Status _ phase r _)) [i] (p1,_,_) =
  clearPhase $ setState newState vm
    where trueInput = fromMaybe r phase
          (left, right) = splitAt' (to rel p1) i state
          newState = left ++ trueInput : tail right

restore :: Action
restore vm@(VM s ip rel  t (Status rs p r _)) [i] (p1,_,_) =
  VM s ip rel  t $ Status rs p r (from s rel i  p1)

jumpIfTrue :: Action
jumpIfTrue vm@(VM state ip rel t s) [c,i] (p1,p2,_) =
  let shouldJump = from state rel c  p1 /= 0
      newIp = from state rel i  p2 - 3 -- correct for outer tick
  in if shouldJump then VM state newIp rel t s else vm

jumpIfFalse :: Action
jumpIfFalse vm@(VM state ip rel t s) [c,i] (p1,p2,_) =
  let shouldJump = from state rel c  p1 == 0
      newIp = from state rel i  p2 - 3 -- correct for outer tick
  in if shouldJump then VM state newIp rel t s else vm

lt :: Action
lt vm@(VM state _ rel _ _) [a,b,i] (p1, p2, p3) = setState newState vm
  where (left, right) = splitAt' (to rel p3) i state
        one = from state rel a  p1
        two = from state rel b  p2
        newState = left ++ (fromEnum (one < two)) : tail right

eq :: Action
eq vm@(VM state _ rel _ _) [a,b,i] (p1, p2, p3) = setState newState vm
  where (left, right) = splitAt' (to rel p3) i state
        one = from state rel a p1
        two = from state rel b p2
        newState = left ++ (fromEnum (one == two)) : tail right

reRel :: Action
reRel vm@(VM state _ rel@(Rel n) _ _) [a] (p1, _, _) = vm { rel = newRel }
  where newRel = Rel $ from state rel a p1 + n

halt :: Action
halt vm _ _ = vm { status = (status vm) { runState = Halted } }

bail :: Action
bail = halt

onState :: ([Int] -> [Int]) -> VM -> VM
onState f vm = vm { state = f . state $ vm }

setState :: [Int] -> VM -> VM
setState state vm = onState (const state) vm

clearPhase :: VM -> VM
clearPhase vm = vm { status = (status vm) { phase = Nothing } }

onIp :: (Int -> Int) -> VM -> VM
onIp f vm = vm { ip = f . ip $ vm }

onTicks :: (Int -> Int) -> VM -> VM
onTicks f (VM state ip rel ticks rs) = VM state ip rel (f ticks) rs

getMode :: Char -> Mode
getMode '0' = Position
getMode '1' = Immediate
getMode '2' = Relative

fill :: String -> String
fill "" = "000"
fill (c:"") = c:"00"
fill (c:b:"") = c:b:"0"
fill cs = cs

toParams :: Int -> Parameters
toParams = (\(a:b:c:_) -> (a,b,c)) . fmap getMode . take 3 . fill . reverse . show

tick :: VM -> VM
tick = onTicks (+1)

execute :: VM -> VM
execute vm@(VM s i r _ _) = onIp (+step) $ op vm args params
  where inst = from s r i Position
        (Opcode _ l op) = fromMaybe bailCode $ M.lookup (inst `rem` 100) opcodes
        args = take l . snd . splitAt (i + 1) $ s
        params = toParams . div inst $ 100
        step = l + 1

run :: VM -> VM
run vm
  | runState (status vm) /= Active = vm
  | otherwise = run $ (execute . tick) vm

type TrackedVM = ([Int], VM)

runFull :: TrackedVM -> TrackedVM
runFull (xs, vm)
  | runState (status vm) == Paused = recurse (xs ++ [readOut vm]) $ unpause vm (readIn vm)
  | runState (status vm) == Halted = (xs, vm)
  | otherwise = recurse xs vm
    where recurse xs' vm' = runFull (xs', (execute . tick) vm')

pause :: VM -> VM
pause vm = vm { status = (status vm) { runState = Paused } }

unpause' :: VM -> VM
unpause' vm = vm { status = (status vm) { runState = Active } }

unpause :: VM -> Int -> VM
unpause vm i
  | runState (status vm) /= Paused = vm
  | otherwise = vm { status = (status vm) { runState = Active, input = i } }

splitAt' :: (Int -> Int) -> Int -> [Int] -> ([Int],[Int])
splitAt' f n' xs
  | len <= n = splitAt n (xs ++ replicate (n + 1 - len) 0)
  | otherwise = splitAt n xs
    where len = length xs
          n = f n'

blankVM :: VM
blankVM = VM [] 0 (Rel 0) 0 $ Status Active Nothing 0 0
