module Language.Bonzai.Syntax.LLIR where
import qualified Data.Text as T

data Segment
  = Function Text [Text] NeededLocalSpace LocalSpace [Instruction]
  | Event Text NeededLocalSpace LocalSpace [Segment] EventQuantity
  | EventOn Int Int Int LocalSpace [Instruction]
  | Instruction Instruction
  deriving (Eq)

type NameAddress = Int
type LibraryAddress = Int
type FunctionIndex = Int
type LocalSpace = [(Text, Int)]
type NeededLocalSpace = Int

type EventQuantity = Int
type InstructionLength = Int

data Comparator
  = LessThan
  | GreaterThan
  | EqualTo
  | NotEqualTo
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  | AndCmp | OrCmp
  deriving (Eq)

data Instruction 
  = LoadLocal Text
  | StoreLocal Text
  | LoadConstant Int
  | LoadGlobal Text
  | StoreGlobal Text
  | LoadNative Int
  | Update
  | Return
  | Compare Comparator
  | MakeList Int
  | MakeEvent EventQuantity InstructionLength LocalSpace
  | ListGet Int
  | Call Int
  | CallGlobal Text Int | CallLocal Text Int | CallNative Int Int
  | JumpIfFalse Int
  | JumpRel Int
  | GetIndex
  | Special
  | Halt
  | Spawn
  | Send Int Int
  | MakeMutable
  | Loc Int Int Int
  | Add | Sub | Mul | Div | Mod
  | TryCatch Int
  | GetValue
  | GetRecordAccess Int
  | MakeRecord Int
  | Break (Maybe Int)
  | Continue (Maybe Int)
  | While [Instruction] [Instruction]
  deriving (Eq)

data Library = MkLibrary Int (Set FunctionLibrary)
data FunctionLibrary = MkFunctionLibrary Text Int NameAddress
  deriving (Eq, Ord)

storeGlobal :: Text -> [Segment]
storeGlobal name = [Instruction (StoreGlobal name)]

loadGlobal :: Text -> [Segment]
loadGlobal name = [Instruction (LoadGlobal name)]

storeLocal :: Text -> [Segment]
storeLocal name = [Instruction (StoreLocal name)]

loadLocal :: Text -> [Segment]
loadLocal name = [Instruction (LoadLocal name)]

loadConstant :: Int -> [Segment]
loadConstant n = [Instruction (LoadConstant n)]

instr :: Instruction -> [Segment]
instr = pure . Instruction

instance ToText Instruction where
  toText (LoadLocal n) = T.concat ["load_local ", n]
  toText (StoreLocal n) = T.concat ["store_local ", n]
  toText (LoadConstant n) = T.concat ["load_constant ", T.pack (show n)]
  toText (LoadGlobal n) = T.concat ["load_global ", n]
  toText (StoreGlobal n) = T.concat ["store_global ", n]
  toText (LoadNative n) = T.concat ["load_native ", T.pack (show n)]
  toText Update = "update"
  toText Return = "return"
  toText (Compare c) = T.concat ["compare ", toText c]
  toText (MakeList n) = T.concat ["make_list ", T.pack (show n)]
  toText (MakeEvent n l _) = T.concat ["make_event ", T.pack (show n), " ", T.pack (show l)]
  toText (ListGet n) = T.concat ["list_get ", T.pack (show n)]
  toText (Call n) = T.concat ["call ", T.pack (show n)]
  toText (CallGlobal n i) = T.concat ["call_global ", n, " ", T.pack (show i)]
  toText (CallLocal n i) = T.concat ["call_local ", n, " ", T.pack (show i)]
  toText (CallNative n i) = T.concat ["call_native ", T.pack (show n), " ", T.pack (show i)]
  toText (JumpIfFalse n) = T.concat ["jump_if_false ", T.pack (show n)]
  toText (JumpRel n) = T.concat ["jump_rel ", T.pack (show n)]
  toText GetIndex = "get_index"
  toText Special = "special"
  toText Halt = "halt"
  toText Spawn = "spawn"
  toText (Send n t) = T.concat ["send ", T.pack (show n), " ", T.pack (show t)]
  toText MakeMutable = "make_mutable"
  toText (Loc n t f) = T.concat ["loc ", T.pack (show n), " ", T.pack (show t), " ", T.pack (show f)]
  toText Add = "add"
  toText Sub = "sub"
  toText Mul = "mul"
  toText Div = "div"
  toText Mod = "mod"
  toText (TryCatch i) = T.concat ["try_catch ", T.pack (show i)]
  toText GetValue = "get_value"
  toText (GetRecordAccess n) = T.concat ["get_record_access ", T.pack (show n)]
  toText (MakeRecord n) = T.concat ["make_record ", T.pack (show n)]
  toText (Break Nothing) = "break"
  toText (Break (Just n)) = T.concat ["break ", T.pack (show n)]
  toText (Continue Nothing) = "continue"
  toText (Continue (Just n)) = T.concat ["continue ", T.pack (show n)]
  toText (While cond body) = T.concat ["while { ", T.intercalate "; " (map toText cond), " } { ", T.intercalate "; " (map toText body), " }"]

instance ToText Segment where
  toText (Function n as _ _ is) = T.concat ["function ", n, "(", T.intercalate ", " as, ") { ", T.intercalate "; " (map toText is), " }"]
  toText (Instruction i) = toText i
  toText (Event n _ _ is _) = T.concat ["event ", n, " { ", T.intercalate "; " (map toText is), " }"]
  toText (EventOn i j k _ instrs) = T.concat ["event_on ", T.pack (show i), " ", T.pack (show j), " ", T.pack (show k), " { ", T.intercalate "; " (map toText instrs), " }"]

instance ToText Comparator where
  toText LessThan = "<"
  toText GreaterThan = ">"
  toText EqualTo = "=="
  toText NotEqualTo = "!="
  toText LessThanOrEqualTo = "<="
  toText GreaterThanOrEqualTo = ">="
  toText AndCmp = "&&"
  toText OrCmp = "||"

instance ToText [Segment] where
  toText = T.intercalate "\n" . map toText