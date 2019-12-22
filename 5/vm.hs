import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.IO
import System.Environment

main = do
  (file:id:_) <- getArgs
  input <- readFile file
  let vm = initVM (read id) . parseState $ input
  print $ run vm

parseState :: String -> [Int]
parseState = fmap (read . T.unpack) . T.splitOn (T.pack ",") . T.pack

initVM :: Int -> [Int] -> VM
initVM input state = VM state 0 0 (Status Active input 0)

data Mode = Position | Immediate deriving (Show)
type Parameters = (Mode, Mode, Mode)
data RunState = Active | Halted deriving (Show)
data Status = Status
  { runState :: RunState
  , input :: Int
  , output :: Int
  } deriving (Show)
data VM = VM
  { state :: [Int]
  , ip :: Int
  , ticks :: Int
  , status :: Status
  } deriving (Show)

data Opcode = Opcode Code Length Action
type Code = Int
type Length = Int
type Action = VM -> [Int] -> Parameters -> VM

opcodes :: M.Map Int Opcode
opcodes = M.fromList
  [ (1,  Opcode 1 3 add)
  , (2,  Opcode 2 3 mult)
  , (3,  Opcode 3 1 store)
  , (4,  Opcode 4 1 restore)
  , (5,  Opcode 5 2 jumpIfTrue)
  , (6,  Opcode 6 2 jumpIfFalse)
  , (7,  Opcode 7 3 lt)
  , (8,  Opcode 7 3 eq)
  , (99, Opcode 99 0 halt)
  , (-1, bailCode)
  ]

bailCode :: Opcode
bailCode = Opcode (-1) 0 bail

from :: [Int] -> Int -> Mode -> Int
from s i Position = s !! i
from _ i Immediate = i

add :: Action
add vm@(VM state _ _ _) [a,b,i] (p1, p2, _) = setState newState vm
  where (left, right) = splitAt i state
        one = from state a p1
        two = from state b p2
        newState = left ++ (one + two) : tail right

mult :: Action
mult vm@(VM state _ _ _) [a,b,i] (p1, p2, _) = setState newState vm
  where (left, right) = splitAt i state
        one = from state a p1
        two = from state b p2
        newState = left ++ (one * two) : tail right

store :: Action
store vm@(VM state _ _ (Status _ r _)) [i] _ = setState newState vm
  where (left, right) = splitAt i state
        newState = left ++ r : tail right

restore :: Action
restore vm@(VM s ip t (Status rs r _)) [i] (p1,_,_) =
  VM s ip t $ Status rs r (from s i p1)

jumpIfTrue :: Action
jumpIfTrue vm@(VM state ip t s) [c,i] (p1,p2,_) =
  let shouldJump = from state c p1 /= 0
      newIp = from state i p2 - 3 -- correct for outer tick
  in if shouldJump then VM state newIp t s else vm

jumpIfFalse :: Action
jumpIfFalse vm@(VM state ip t s) [c,i] (p1,p2,_) =
  let shouldJump = from state c p1 == 0
      newIp = from state i p2 - 3 -- correct for outer tick
  in if shouldJump then VM state newIp t s else vm

lt :: Action
lt vm@(VM state _ _ _) [a,b,i] (p1, p2, _) = setState newState vm
  where (left, right) = splitAt i state
        one = from state a p1
        two = from state b p2
        newState = left ++ (fromEnum (one < two)) : tail right

eq :: Action
eq vm@(VM state _ _ _) [a,b,i] (p1, p2, _) = setState newState vm
  where (left, right) = splitAt i state
        one = from state a p1
        two = from state b p2
        newState = left ++ (fromEnum (one == two)) : tail right

halt :: Action
halt (VM s i t (Status _ r w)) _ _ = VM s i t $ Status Halted r w

bail :: Action
bail = halt

onState :: ([Int] -> [Int]) -> VM -> VM
onState f (VM state ip ticks rs) = VM (f state) ip ticks rs

setState :: [Int] -> VM -> VM
setState state vm = onState (const state) vm

onIp :: (Int -> Int) -> VM -> VM
onIp f (VM state ip ticks rs) = VM state (f ip) ticks rs

onTicks :: (Int -> Int) -> VM -> VM
onTicks f (VM state ip ticks rs) = VM state ip (f ticks) rs

getMode :: Char -> Mode
getMode '0' = Position
getMode '1' = Immediate

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
execute vm@(VM state ip t r) = onIp (+step) $ op vm args params
  where inst = state !! ip
        (Opcode _ l op) = fromMaybe bailCode $ M.lookup (inst `rem` 100) opcodes
        args = take l . snd . splitAt (ip + 1) $ state
        params = toParams . div inst $ 100
        step = l + 1

run :: VM -> VM
run vm@(VM _ _ _ (Status Halted _ _)) = vm
run vm = run $ (execute . tick) vm
