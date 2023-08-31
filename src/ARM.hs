{-# LANGUAGE GeneralisedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module ARM
    ( ARM

    -- TODO: organise
    , genASM
    , (#)
    , toLabel

    -- registers
    , r0

    -- register synonyms
    , fp
    , lr
    , pc

    -- instructions
    , add
    , mov
    , push
    , pop

    -- directives
    , global

    -- misc
    , comment
    , label
    )
    where
--------------------------------------------------------------------------------
import           Control.Monad.RWS
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.List                  (intersperse)
import           Data.Coerce                (coerce)
import           GHC.Exts                   (IsString)
--------------------------------------------------------------------------------
newtype ARM a = ARM { runARM :: RWS () [Instruction] Int a }
    deriving (Functor, Applicative, Monad)

genASM :: ARM a -> Text
genASM arm =
    let (_,_,w) = runRWS (runARM arm) () 0
    in T.unlines $ fmap asmInstruction w

newtype Label = Label Text
    deriving (IsString)
type Immediate = Int

newlabel :: ARM Label
newlabel = ARM $ do
    n <- get
    modify succ
    pure $ Label . T.pack $ ("L" <> show n)

class FlexibleRegSet a where
    toRegSet :: a -> RegSet

instance FlexibleRegSet Reg where
    toRegSet = RegSingle

instance FlexibleRegSet [Reg] where
    toRegSet = RegSet

class FlexibleOperand a where
    toOperand2 :: a -> Operand2

instance FlexibleOperand Immediate where
    -- TODO: The constant must correspond to an 8-bit pattern rotated by an even
    -- number of bits within a 32-bit word
    toOperand2 = FlexImmediate

instance FlexibleOperand Reg where
    toOperand2 r = FlexRegister r Nothing

instance FlexibleOperand (Reg, Shift) where
    toOperand2 (r,s) = FlexRegister r (Just s)

--------------------------------------------------------------------------------

-- not necessarily an instruction. an Instruction represents some line of
-- assembly output, such as a comment, a directive, a label, or a real
-- mnemonic instruction.
data Instruction where
    -- not real instructions
    Comment     :: String       -> Instruction
    Directive   :: Directive    -> Instruction
    LabelInstr  :: Label        -> Instruction

    Add     :: Reg -> Reg       -> Operand2 -> Instruction
    Mov     :: Reg -> Operand2              -> Instruction

    Push    :: RegSet -> Instruction
    Pop     :: RegSet -> Instruction

data Directive = Global Label

data RegSet = RegSet [Reg]
            | RegSingle Reg

data Reg = R0  | R1  | R2  | R3
         | R4  | R5  | R6  | R7
         | R8  | R9  | R10 | R11
         | R12 | R13 | R14 | R15

r0 :: Reg
r0 = R0

fp, lr, pc :: Reg
fp = R11
lr = R14
pc = R15

data Shift = ASR Int
           | LSL Int
           | LSR Int
           | ROR Int
           | RRX

-- https://developer.arm.com/documentation/dui0068/b/ARM-Instruction-Reference/ARM-general-data-processing-instructions/ADD--SUB--RSB--ADC--SBC--and-RSC?lang=en
data Operand2 = FlexImmediate Immediate
              | FlexRegister Reg (Maybe Shift)

--------------------------------------------------------------------------------

asmInstruction :: Instruction -> Text

-- TODO: ensure comment isnt escaped
asmInstruction (Comment s) = "/* " <> T.pack s <> " */"

asmInstruction (Directive d) = "." <> asmDirective d

asmInstruction (LabelInstr l) = coerce l <> ":"

-- mnemonics
asmInstruction mn = case mn of
    -- (Mov rd op2)    -> "mov  " <> asmReg rd <> " " <> asmOp2 op2
    -- (Push rs)       -> "push " <> asmRegSet rs
    -- (Pop  rs)       -> "pop  " <> asmRegSet rs
    (Mov rd op2)    -> mnemonic "mov" [asmReg rd, asmOp2 op2]
    (Push rs)       -> mnemonic "push" [asmRegSet rs]
    (Pop  rs)       -> mnemonic "pop" [asmRegSet rs]

    where
        mnemonic s ops = "\t" <> align s <> " "
            <> (mconcat . intersperse ", " $ ops)
        
        align s = s <> (T.pack $ replicate (max (5 - T.length s) 0) ' ')

asmDirective :: Directive -> Text
asmDirective d = case d of
    Global (Label l) -> "global " <> l

asmReg :: Reg -> Text
asmReg r = case r of
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
    R11 -> "fp"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "lr"
    R15 -> "pc"

asmRegSet :: RegSet -> Text
asmRegSet (RegSingle r) = asmReg r
asmRegSet (RegSet rs) = "{" <> mconcat (intersperse ", " regs) <> "}"
    where regs = fmap asmReg rs

asmOp2 :: Operand2 -> Text
asmOp2 (FlexImmediate n) = T.pack $ "#" ++ show n
asmOp2 (FlexRegister r Nothing) = asmReg r
asmOp2 (FlexRegister r (Just s)) = asmReg r <> ", " <> asmShift s

asmShift :: Shift -> Text
asmShift s = case s of
    ASR n   -> "ASR " <> imm n
    LSL n   -> "LSL " <> imm n
    LSR n   -> "LSR " <> imm n
    ROR n   -> "ROR " <> imm n
    RRX     -> "RRX"

    where imm = asmOp2 . FlexImmediate

--------------------------------------------------------------------------------

emiti :: Instruction -> ARM ()
emiti = ARM . tell . pure

toLabel :: String -> Label
toLabel = Label . T.pack

--------------------------------------------------------------------------------

comment :: String -> ARM ()
comment s = emiti $ Comment s

label :: Label -> ARM ()
label l = emiti $ LabelInstr l

--------------------------------------------------------------------------------

global :: Label -> ARM ()
global l = emiti $ Directive $ Global l

--------------------------------------------------------------------------------

add :: (FlexibleOperand op2) => Reg -> Reg -> op2 -> ARM ()
add rd rs op2 = emiti $ Add rd rs (toOperand2 op2)

mov :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
mov rd op2 = emiti $ Mov rd (toOperand2 op2)

push :: (FlexibleRegSet a) => a -> ARM ()
push rs = emiti $ Push (toRegSet rs)

pop :: (FlexibleRegSet a) => a -> ARM ()
pop rs = emiti $ Pop (toRegSet rs)

infixl 9 #

-- | used to avoid ambiguity errors with Num and FlexibleOperand
-- in a way that is remenicent of gas's arm syntax
(#) :: (Immediate -> b) -> Immediate -> b
f # x = f x

