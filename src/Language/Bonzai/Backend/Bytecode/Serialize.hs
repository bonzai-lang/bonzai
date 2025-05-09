module Language.Bonzai.Backend.Bytecode.Serialize where
import Language.Bonzai.Syntax.Bytecode
import Language.Bonzai.Syntax.LLIR hiding (Instruction(..), Segment(..))
import Language.Bonzai.Syntax.Internal.Literal
import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import qualified Data.ByteString.Lazy as BSL

-- | BYTECODE SERIALIZATION
-- | Bytecode serialization is the process of transforming the bytecode AST into
-- | a binary format that can be read by the virtual machine.
-- |
-- | The serialization process is done in multiple steps:
-- |
-- |  - Encode integer: This step encodes an integer into a binary format.
-- |
-- |  - Encode comparator: This step encodes a comparator into a binary format.
-- |
-- |  - Encode n null(s): This step encodes n null(s) value(s) into a 
-- |    binary format.
-- |
-- |  - Encode instruction: This step encodes an instruction into a binary
-- |    format. An instruction is just an int32 value.
-- |
-- |  - Encode text: This step encodes a text value into a binary format.
-- |
-- |  - Encode constant: This step encodes a constant value into a binary
-- |    format.
-- |
-- |  - Encode native: This step encodes a native library into a binary format.
-- |
-- |  - Encode program: This step encodes a program into a binary format.

encodeInteger :: (Integral a) => a -> Put
encodeInteger = putInt32le . fromIntegral

encodeComparator :: Comparator -> Put
encodeComparator LessThan = putInt32le 0
encodeComparator GreaterThan = putInt32le 1
encodeComparator EqualTo = putInt32le 2
encodeComparator NotEqualTo = putInt32le 3
encodeComparator LessThanOrEqualTo = putInt32le 4
encodeComparator GreaterThanOrEqualTo = putInt32le 5
encodeComparator AndCmp = putInt32le 6
encodeComparator OrCmp = putInt32le 7

encodeNull :: Put
encodeNull = putInt32le 0

replicateNull :: Int -> Put
replicateNull = flip replicateM_ encodeNull

encodeInstr :: Int -> Put
encodeInstr = putInt32le . fromIntegral

encodeInstruction :: Instruction -> Put
encodeInstruction (LoadLocal i) = 
  encodeInstr 0 >> encodeInteger i >> replicateNull 3
encodeInstruction (StoreLocal i) = 
  encodeInstr 1 >> encodeInteger i >> replicateNull 3
encodeInstruction (LoadConstant i) = 
  encodeInstr 2 >> encodeInteger i >> replicateNull 3
encodeInstruction (LoadGlobal i) = 
  encodeInstr 3 >> encodeInteger i >> replicateNull 3
encodeInstruction (StoreGlobal i) = 
  encodeInstr 4 >> encodeInteger i >> replicateNull 3
encodeInstruction Return = 
  encodeInstr 5 >> replicateNull 4
encodeInstruction (Compare c) = 
  encodeInstr 6 >> encodeComparator c >> replicateNull 3
encodeInstruction Update =
  encodeInstr 7 >> replicateNull 4
encodeInstruction (MakeList i) =
  encodeInstr 8 >> encodeInteger i >> replicateNull 3
encodeInstruction (ListGet i) =
  encodeInstr 9 >> encodeInteger i >> replicateNull 3
encodeInstruction (Call i) =
  encodeInstr 10 >> encodeInteger i >> replicateNull 3
encodeInstruction (CallGlobal i j) =
  encodeInstr 11 >> encodeInteger i >> encodeInteger j >> replicateNull 2
encodeInstruction (CallLocal i j) =
  encodeInstr 12 >> encodeInteger i >> encodeInteger j >> replicateNull 2
encodeInstruction (JumpIfFalse i) =
  encodeInstr 13 >> encodeInteger i >> replicateNull 3
encodeInstruction (JumpRel i) =
  encodeInstr 14 >> encodeInteger i >> replicateNull 3
encodeInstruction GetIndex =
  encodeInstr 15 >> replicateNull 4
encodeInstruction Special =
  encodeInstr 16 >> replicateNull 4
encodeInstruction Halt =
  encodeInstr 17 >> replicateNull 4
encodeInstruction Spawn =
  encodeInstr 18 >> replicateNull 4
encodeInstruction (EventOn i j k ls) =
  encodeInstr 19 >> encodeInteger i >> encodeInteger j >> encodeInteger k >> encodeInteger ls
encodeInstruction (Send i j) =
  encodeInstr 20 >> encodeInteger i >> encodeInteger j >> replicateNull 2
encodeInstruction (MakeFunctionAndStore i j k) =
  encodeInstr 21 >> encodeInteger i >> encodeInteger j >> encodeInteger k >> replicateNull 1
encodeInstruction (LoadNative i) =
  encodeInstr 22 >> encodeInteger i >> replicateNull 3
encodeInstruction (MakeEvent i j) = 
  encodeInstr 23 >> encodeInteger i >> encodeInteger j >> replicateNull 2
encodeInstruction ReturnEvent =
  encodeInstr 24 >> replicateNull 4
encodeInstruction MakeMutable =
  encodeInstr 25 >> replicateNull 4
encodeInstruction (Loc i j k) =
  encodeInstr 26 >> encodeInteger i >> encodeInteger j >> encodeInteger k >> replicateNull 1
encodeInstruction Add =
  encodeInstr 27 >> replicateNull 4
encodeInstruction Sub =
  encodeInstr 28 >> replicateNull 4
encodeInstruction Mul =
  encodeInstr 29 >> replicateNull 4
encodeInstruction Div =
  encodeInstr 30 >> replicateNull 4
encodeInstruction Mod =
  encodeInstr 31 >> replicateNull 4
encodeInstruction (CallNative i j) =
  encodeInstr 32 >> encodeInteger i >> encodeInteger j >> replicateNull 2
encodeInstruction (TryCatch i) =
  encodeInstr 33 >> encodeInteger i >> replicateNull 3
encodeInstruction GetValue =
  encodeInstr 34 >> replicateNull 4
encodeInstruction (GetRecordAccess i) =
  encodeInstr 35 >> encodeInteger i >> replicateNull 3

encodeText :: Text -> Put
encodeText w = do
  encodeInteger $ BS.length encoded
  putByteString encoded
 where
  encoded = encodeUtf8 w

encodeConstant :: Literal -> Put
encodeConstant (MkLitInt i) = putWord8 0 >> encodeInteger i
encodeConstant (MkLitFloat f) = putWord8 1 >> putDoublele f
encodeConstant (MkLitString t) = putWord8 2 >> encodeText t
encodeConstant (MkLitChar c) = putWord8 2 >> encodeText (fromString [c])
encodeConstant (MkLitBool True) = putWord8 0 >> encodeInteger (1 :: Int)
encodeConstant (MkLitBool False) = putWord8 0 >> encodeInteger (0 :: Int)

runSerializer :: [Instruction] -> [Literal] -> BSL.ByteString
runSerializer xs lits = runPut $ do
  encodeInteger $ length lits
  mapM_ encodeConstant lits

  encodeInteger $ length xs
  mapM_ encodeInstruction xs

