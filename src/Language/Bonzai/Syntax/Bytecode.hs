{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Bonzai.Syntax.Bytecode where
import qualified Language.Bonzai.Syntax.LLIR as LLIR
import qualified Data.Text as T

data Instruction 
  = LoadLocal Int
  | StoreLocal Int
  | LoadConstant Int
  | LoadGlobal Int
  | StoreGlobal Int
  | LoadNative Int
  | Update
  | Return
  | Compare LLIR.Comparator
  | MakeList Int
  | MakeEvent LLIR.EventQuantity LLIR.InstructionLength
  | ListGet Int
  | Call Int
  | CallGlobal Int Int | CallLocal Int Int | CallNative Int Int
  | JumpIfFalse Int
  | JumpRel Int
  | GetIndex
  | Special
  | Halt
  | Spawn
  | EventOn Int Int Int LLIR.NeededLocalSpace
  | Send Int Int
  | MakeFunctionAndStore LLIR.NameAddress LLIR.InstructionLength LLIR.NeededLocalSpace
  | ReturnEvent
  | MakeMutable
  | Loc Int Int Int
  | Add | Sub | Mul | Div | Mod
  | TryCatch Int
  | GetValue
  | GetRecordAccess Int
  | MakeRecord Int
  | Jump Int
  deriving (Eq)

instance ToText Int where
  toText = T.pack . show

instance ToText Instruction where
  toText (LoadLocal a) = "LoadLocal " <> toText a
  toText (StoreLocal a) = "StoreLocal " <> toText a
  toText (LoadConstant a) = "LoadConstant " <> toText a
  toText (LoadGlobal a) = "LoadGlobal " <> toText a
  toText (StoreGlobal a) = "StoreGlobal " <> toText a
  toText (LoadNative a) = "LoadNative " <> toText a
  toText Update = "Update"
  toText Return = "Return"
  toText (Compare a) = "Compare " <> toText a
  toText (MakeList a) = "MakeList " <> toText a
  toText (MakeEvent a c) = "MakeEvent " <> toText a <> " " <> toText c
  toText (ListGet a) = "ListGet " <> toText a
  toText (Call a) = "Call " <> toText a
  toText (CallGlobal a b) = "CallGlobal " <> toText a <> " " <> toText b
  toText (CallLocal a b) = "CallLocal " <> toText a <> " " <> toText b
  toText (CallNative a b) = "CallNative " <> toText a <> " " <> toText b
  toText (JumpIfFalse a) = "JumpIfFalse " <> toText a
  toText (JumpRel a) = "JumpRel " <> toText a
  toText GetIndex = "GetIndex"
  toText Special = "Special"
  toText Halt = "Halt"
  toText Spawn = "Spawn"
  toText (EventOn a b c _) = "EventOn " <> toText a <> " " <> toText b <> " " <> toText c
  toText (Send a b) = "Send " <> toText a <> " " <> toText b
  toText (MakeFunctionAndStore a b c) = "MakeFunctionAndStore " <> toText a <> " " <> toText b <> " " <> toText c
  toText ReturnEvent = "ReturnEvent"
  toText MakeMutable = "MakeMutable"
  toText (Loc a b c) = "Loc " <> toText a <> " " <> toText b <> " " <> toText c
  toText Add = "Add"
  toText Sub = "Sub"
  toText Mul = "Mul"
  toText Div = "Div"
  toText Mod = "Mod"
  toText (TryCatch a) = "TryCatch " <> toText a
  toText GetValue = "GetValue"
  toText (GetRecordAccess a) = "GetRecordAccess " <> toText a
  toText (MakeRecord a) = "MakeRecord " <> toText a
  toText (Jump a) = "Jump " <> toText a

instance ToText [Instruction] where
  toText xs = T.intercalate "\n" . map format $ couple
    where couple = zip [(0 :: Int)..] xs
          format (i, x) = T.concat [toText i, ": ", toText x]