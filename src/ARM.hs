{-# LANGUAGE GeneralisedNewtypeDeriving, OverloadedStrings #-}
module ARM
    ( 

    )
    where
--------------------------------------------------------------------------------
import           Control.Monad.RWS
import qualified Data.Text                  as T
import           Data.Text                  (Text)
--------------------------------------------------------------------------------
newtype ARM a = ARM { runARM :: RWS () [Instruction] Int a }
    deriving (Functor, Applicative, Monad)

newtype Label = Label Text

label :: ARM Label
label = ARM $ do
    n <- get
    modify succ
    pure $ Label . T.pack $ ("L" <> show n)

-- class InstrAdd a where
--     add :: Reg -> Reg -> a -> ARM ()

-- instance InstrAdd Int where
--     add p q i = ARM $ tell [Add p q (FlexImmediate i)]

-- instance InstrAdd Reg where
--     add p q r = ARM $ tell [Add p q (FlexRegister r Nothing)]

class FlexibleOperand a where
    toOperand2 :: a -> Operand2

instance FlexibleOperand Int where
    toOperand2 = FlexImmediate

instance FlexibleOperand Reg where
    toOperand2 r = FlexRegister r Nothing

instance FlexibleOperand (Reg, Shift) where
    toOperand2 (r,s) = FlexRegister r (Just s)

--------------------------------------------------------------------------------

data Instruction where
    Add :: Reg -> Reg -> Operand2 -> Instruction

data Reg = R0  | R1  | R2  | R3
         | R4  | R5  | R6  | R7
         | R8  | R9  | R10 | R11
         | R12 | R13 | R14 | R15

data Shift = ASR Int
           | LSL Int
           | LSR Int
           | ROR Int
           | RRX

-- https://developer.arm.com/documentation/dui0068/b/ARM-Instruction-Reference/ARM-general-data-processing-instructions/ADD--SUB--RSB--ADC--SBC--and-RSC?lang=en
data Operand2 = FlexImmediate Int
              | FlexRegister Reg (Maybe Shift)

--------------------------------------------------------------------------------

class ToASM a where
    -- returned text is not necessarily an instruction
    toASM :: a -> Text

instance ToASM Reg where
    toASM r = case r of
        R0 -> "r0"
        R1 -> "r1"
        R2 -> "r2"
        R3 -> "r3"
        R4 -> "r4"
        R5 -> "r5"
        R6 -> "r6"
        R7 -> "r7"
        R8 -> "r8"
        R9 -> "r9"
        R10 -> "r10"
        R11 -> "r11"
        R12 -> "r12"
        R13 -> "r13"
        R14 -> "r14"
        R15 -> "r15"

instance ToASM Operand2 where
    toASM (FlexImmediate n) = T.pack $ "#" ++ show n
    toASM (FlexRegister r Nothing) = toASM r
    toASM (FlexRegister r (Just s)) = toASM r <> ", " <> toASM s

instance ToASM Shift where
    toASM s = case s of
        ASR n   -> "ASR " <> imm n
        LSL n   -> "LSL " <> imm n
        LSR n   -> "LSR " <> imm n
        ROR n   -> "ROR " <> imm n
        RRX     -> "RRX"

        where imm = toASM . FlexImmediate

