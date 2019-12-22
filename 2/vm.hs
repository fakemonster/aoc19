import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.IO
import System.Environment

main = do
  input <- (head <$> getArgs) >>= readFile
  let vm = initVM . parseState $ input
  print $ run vm

parseState :: String -> [Int]
parseState = fmap (read . T.unpack) . T.splitOn (T.pack ",") . T.pack

initVM :: [Int] -> VM
initVM state = VM state 0 0 Active

data RunState = Active | Halted deriving (Show)
data VM = VM
  { state :: [Int]
  , ip :: Int
  , ticks :: Int
  , runState :: RunState
  } deriving (Show)

data Opcode = Opcode Code Length Action
type Code = Int
type Length = Int
type Action = VM -> [Int] -> VM

add :: Action
add vm@(VM state _ _ _) [a,b,c] = setState newState vm
  where (left, right) = splitAt c state
        one = state !! a
        two = state !! b
        newState = left ++ (one + two) : tail right

mult :: Action
mult vm@(VM state _ _ _) [a,b,c] = setState newState vm
  where (left, right) = splitAt c state
        one = state !! a
        two = state !! b
        newState = left ++ (one * two) : tail right

halt :: Action
halt (VM s i t _) _ = VM s i t Halted

bail :: Action
bail = halt

bailCode :: Opcode
bailCode = Opcode (-1) 0 bail

opcodes :: M.Map Int Opcode
opcodes = M.fromList
  [ (1,  Opcode 1 3 add)
  , (2,  Opcode 2 3 mult)
  , (99, Opcode 99 0 halt)
  , (-1, bailCode)
  ]

onState :: ([Int] -> [Int]) -> VM -> VM
onState f (VM state ip ticks rs) = VM (f state) ip ticks rs

setState :: [Int] -> VM -> VM
setState state vm = onState (const state) vm

onIp :: (Int -> Int) -> VM -> VM
onIp f (VM state ip ticks rs) = VM state (f ip) ticks rs

onTicks :: (Int -> Int) -> VM -> VM
onTicks f (VM state ip ticks rs) = VM state ip (f ticks) rs

tick :: VM -> VM
tick = onTicks (+1)

execute :: VM -> VM
execute vm@(VM state ip t r) = onIp (+step) $ op vm args
  where (Opcode _ l op) = fromMaybe bailCode $ M.lookup (state !! ip) opcodes
        args = take l . snd . splitAt (ip + 1) $ state
        step = l + 1

run :: VM -> VM
run vm@(VM _ _ _ Halted) = vm
run vm = run $ execute vm
