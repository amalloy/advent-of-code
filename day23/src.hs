import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Control.Arrow ((&&&))
import Data.Bool
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Combinator

type Register = Char
type Value = Int
type Offset = Int

data Instruction = RegisterInstr (Value -> Value) Register
                 | JumpInstr Offset | ConditionlalJumpInstr Register (Value -> Bool) Offset

instance Show Instruction where
  show (RegisterInstr f r) = "reg " ++ [r]
  show (JumpInstr o) = "jmp " ++ (show o)
  show (ConditionlalJumpInstr r f o) = "jif " ++ [r] ++ ", " ++ (show o)

type Program = M.Map Int Instruction
type Registers = M.Map Register Value

data Computer = Computer {ip :: Int,
                          registers :: Registers,
                          program :: Program
                          }
type Operation = State Computer ()

firstAddress :: Int
firstAddress = 0

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
  v <- gets (reg r)
  jumpBy . bool 1 offset $ pred v
doInstruction (RegisterInstr f r) = do
  v <- gets (reg r)
  modify $ changeIP 1 . writeReg r (f v)

runProgram :: Operation
runProgram = do
  instr <- gets currentInstruction
  case instr of
    Nothing -> return () -- IP out of bounds, so terminate
    (Just instr) -> doInstruction instr >> runProgram

instruction :: CharParser () Instruction
instruction = regInstr "tpl" (* 3) <|>
              regInstr "hlf" (`div` 2) <|>
              regInstr "inc" (+ 1) <|>
              jmp <|>
              jumpInstr "jie" even <|>
              jumpInstr "jio" (== 1)

offset :: CharParser () Offset
offset = do
  sign <- (char '-' >> return negate) <|> (char '+' >> return id)
  sign . read <$> many digit

parseLabel :: String -> CharParser () ()
parseLabel s = try (string s) >> char ' ' >> return ()

jmp :: CharParser () Instruction
jmp = parseLabel "jmp" >> JumpInstr <$> offset

jumpInstr :: String -> (Value -> Bool) -> CharParser () Instruction
jumpInstr name pred = do
  parseLabel name
  reg <- anyChar
  string ", "
  ConditionlalJumpInstr reg pred <$> offset

regInstr :: String -> (Value -> Value) -> CharParser () Instruction
regInstr name f = parseLabel name >> RegisterInstr f <$> anyChar

parseProgram :: CharParser () Program
parseProgram = do
  instrs <- instruction `sepEndBy` newline
  return $ M.fromList (zip [firstAddress..] instrs)

-- partial function, only works because input is always well formed
doParse :: CharParser () a -> String -> a
doParse p s = case runParser p () "input" s of
  (Left e) -> error ":("
  (Right x) -> x

load :: Program -> Computer
load = Computer firstAddress (M.fromList $ zip ['a'..'z'] (repeat 0))

solve :: Register -> Computer -> Value
solve r c = evalState (runProgram >> gets (reg r)) c

main = interact $ show . (solve 'b' &&& solve 'b' . writeReg 'a' 1)
                         . load . doParse parseProgram
