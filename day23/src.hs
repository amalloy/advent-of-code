import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad

type Register = String
type Value = Int
type Offset = Int

data Instruction = RegisterInstr (Value -> Value) Register
                 | JumpInstr Offset | ConditionlalJumpInstr Register (Value -> Bool) Offset

type Program = M.Map Int Instruction
type Registers = M.Map String Value

data Computer = Computer {program :: Program,
                          registers :: Registers,
                          ip :: Int}
type Operation = State Computer ()

currentInstruction :: Computer -> Maybe Instruction
currentInstruction = M.lookup <$> ip <*> program

reg :: Register -> Computer -> Value
reg r = (M.! r) <$> registers

writeReg :: Register -> Value -> Computer -> Computer
writeReg r v c = c {registers = M.insert r v (registers c)}

changeIP :: Offset -> Computer -> Computer
changeIP offset c = c {ip = ip c + offset}

jumpBy :: Int -> Operation
jumpBy offset = modify $ changeIP offset

doInstruction :: Instruction -> Operation
doInstruction (JumpInstr offset) = jumpBy offset
doInstruction (ConditionlalJumpInstr r pred offset) = do
  v <- reg r <$> get
  when (pred v) $ jumpBy offset
doInstruction (RegisterInstr f r) = do
  v <- reg r <$> get
  modify $ changeIP 1 . writeReg r (f v)
