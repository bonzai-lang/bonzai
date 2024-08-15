module Language.Bonzai.Backend.Bytecode.Serialize where
import Language.Bonzai.Syntax.Bytecode
import Language.Bonzai.Syntax.LLIR hiding (Instruction(..))
import Language.Bonzai.Syntax.Internal.Literal
import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import qualified Data.ByteString.Lazy as BSL

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
  encodeInstr 0 >> encodeInteger i >> replicateNull 2
encodeInstruction (StoreLocal i) = 
  encodeInstr 1 >> encodeInteger i >> replicateNull 2
encodeInstruction (LoadConstant i) = 
  encodeInstr 2 >> encodeInteger i >> replicateNull 2
encodeInstruction (LoadGlobal i) = 
  encodeInstr 3 >> encodeInteger i >> replicateNull 2
encodeInstruction (StoreGlobal i) = 
  encodeInstr 4 >> encodeInteger i >> replicateNull 2
encodeInstruction Return = 
  encodeInstr 5 >> replicateNull 3
encodeInstruction (Compare c) = 
  encodeInstr 6 >> encodeComparator c >> replicateNull 2
encodeInstruction Update =
  encodeInstr 7 >> replicateNull 3
encodeInstruction (MakeList i) =
  encodeInstr 8 >> encodeInteger i >> replicateNull 2
encodeInstruction (ListGet i) =
  encodeInstr 9 >> encodeInteger i >> replicateNull 2
encodeInstruction (Call i) =
  encodeInstr 10 >> encodeInteger i >> replicateNull 2
encodeInstruction (CallGlobal i j) =
  encodeInstr 11 >> encodeInteger i >> encodeInteger j
encodeInstruction (CallLocal i j) =
  encodeInstr 12 >> encodeInteger i >> encodeInteger j
encodeInstruction (JumpIfFalse i) =
  encodeInstr 13 >> encodeInteger i >> replicateNull 2
encodeInstruction (JumpRel i) =
  encodeInstr 14 >> encodeInteger i >> replicateNull 2
encodeInstruction GetIndex =
  encodeInstr 15 >> replicateNull 3
encodeInstruction Special =
  encodeInstr 16 >> replicateNull 3
encodeInstruction Halt =
  encodeInstr 17 >> replicateNull 3
encodeInstruction Spawn =
  encodeInstr 18 >> replicateNull 3
encodeInstruction (EventOn i j k) =
  encodeInstr 19 >> encodeInteger i >> encodeInteger j >> encodeInteger k
encodeInstruction (Send i j) =
  encodeInstr 20 >> encodeInteger i >> encodeInteger j >> encodeNull
encodeInstruction (MakeFunctionAndStore i j k) =
  encodeInstr 21 >> encodeInteger i >> encodeInteger j >> encodeInteger k
encodeInstruction (LoadNative i) =
  encodeInstr 22 >> encodeInteger i >> replicateNull 2
encodeInstruction (MakeEvent i j k) = 
  encodeInstr 23 >> encodeInteger i >> encodeInteger j >> encodeInteger k
encodeInstruction ReturnEvent =
  encodeInstr 24 >> replicateNull 3
encodeInstruction MakeMutable =
  encodeInstr 25 >> replicateNull 3

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

runSerializer :: [Instruction] -> [Literal] -> BSL.ByteString
runSerializer xs lits = runPut $ do
  encodeInteger $ length lits
  mapM_ encodeConstant lits

  encodeInteger $ length xs
  mapM_ encodeInstruction xs
