{-# LANGUAGE GeneralisedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures, KindSignatures, TypeFamilies, DataKinds, PolyKinds #-}
module ARM
    ( ARM
    
    -- TODO: organise
    , genASM
    , (#)
    , toLabel
    , allocLabel

    -- registers
    , Reg
    , r0
    , r1
    , r2
    , r3

    -- register synonyms
    , fp
    , lr
    , pc
    , ip
    , sp

    -- instructions
    , add
    , sub
    , mul
    , udiv
    , mov
    , moveq
    , movne
    , movlt
    , movge
    , push
    , pop
    , cmp
    , bl
    , branch
    , beq
    , bne
    , ldr
    , str

    -- aliases
    , pusha

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
import           Data.Char                  (ord)
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

data Immediate = ImmInt Int
               | ImmChar Char

allocLabel :: ARM Label
allocLabel = ARM $ do
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

instance FlexibleOperand Int where
    -- TODO: The constant must correspond to an 8-bit pattern rotated by an even
    -- number of bits within a 32-bit word
    toOperand2 = FlexImmediate . ImmInt

instance FlexibleOperand Char where
    toOperand2 = FlexImmediate . ImmChar

instance FlexibleOperand Reg where
    toOperand2 r = FlexRegister r Nothing

instance FlexibleOperand (Reg, Shift) where
    toOperand2 (r,s) = FlexRegister r (Just s)

--------------------------------------------------------------------------------

data MemOp = DerefReg Reg
           | MemAddress Int -- todo: word32
           | MemLabel Label
           | MemOffset (Reg, FlexOffset)

class FlexibleMemOp a where
    toMemOp :: a -> MemOp

type DereferencedReg = [Reg]

instance FlexibleMemOp DereferencedReg where
    toMemOp [r] = DerefReg r
    toMemOp _ = error "list instance is only for syntax :("

type FlexOffset = Int

instance FlexibleMemOp (Reg, FlexOffset) where
    toMemOp = MemOffset

instance FlexibleMemOp Int where
    toMemOp = MemAddress

instance FlexibleMemOp Label where
    toMemOp = MemLabel
--------------------------------------------------------------------------------

-- not necessarily an instruction. an Instruction represents some line of
-- assembly output, such as a comment, a directive, a label, or a real
-- mnemonic instruction.
data Instruction where
    -- not real instructions
    Comment     :: String       -> Instruction
    Directive   :: Directive    -> Instruction
    LabelInstr  :: Label        -> Instruction

    Add     ::              Reg -> Reg       -> Operand2 -> Instruction
    Sub     ::              Reg -> Reg       -> Operand2 -> Instruction
    Mul     ::              Reg -> Reg       -> Reg      -> Instruction
    Udiv    ::              Reg -> Reg       -> Reg      -> Instruction
    Mov     :: Condition -> Reg -> Operand2              -> Instruction
    Cmp     ::              Reg -> Operand2              -> Instruction

    -- memory
    Ldr     ::              Reg -> MemOp                 -> Instruction
    Str     :: Condition -> Reg -> MemOp                 -> Instruction

    Branch  :: Condition -> Label -> Instruction
    Bl      ::              Label -> Instruction

    Push    :: RegSet -> Instruction
    Pop     :: RegSet -> Instruction

data Condition = Uncond
               | CondEQ
               | CondNE
               | CondLT
               | CondGE

data Directive = Global Label

data RegSet = RegSet [Reg]
            | RegSingle Reg

data Reg = R0  | R1  | R2  | R3
         | R4  | R5  | R6  | R7
         | R8  | R9  | R10 | R11
         | R12 | R13 | R14 | R15
         deriving Eq

r0, r1, r2, r3 :: Reg
r0 = R0
r1 = R1
r2 = R2
r3 = R3

fp, lr, pc :: Reg
fp = R11
ip = R12
sp = R13
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
    (Add rd rn op2)  -> mnemonic "add" Uncond [asmReg rd, asmReg rn, asmOp2 op2]
    (Sub rd rn op2)  -> mnemonic "sub" Uncond [asmReg rd, asmReg rn, asmOp2 op2]
    (Mul rd rm rs)   -> mnemonic "mul" Uncond [asmReg rd, asmReg rm, asmReg rs]
    (Udiv rd rm rs)  -> mnemonic "udiv" Uncond [asmReg rd, asmReg rm, asmReg rs]
    (Mov cnd rd op2) -> mnemonic "mov" cnd [asmReg rd, asmOp2 op2]
    (Push rs)        -> mnemonic "push" Uncond [asmRegSet rs]
    (Pop  rs)        -> mnemonic "pop" Uncond [asmRegSet rs]
    (Cmp   rn op2)   -> mnemonic "cmp" Uncond [asmReg rn, asmOp2 op2]
    (Ldr   rd mop)   -> mnemonic "ldr" Uncond [asmReg rd, asmMop mop]
    (Str c rs mop)   -> mnemonic "str" c [asmReg rs, asmMop mop]
    (Branch cnd l)   -> mnemonic "b" cnd [coerce l]
    (Bl         l)   -> mnemonic "bl" Uncond [coerce l]

    where
        mnemonic s cnd ops = "\t" <> align (s <> condsuffix cnd) <> " "
            <> (mconcat . intersperse ", " $ ops)
        
        align s = s <> (T.pack $ replicate (max (7 - T.length s) 0) ' ')

        condsuffix a = case a of
            Uncond -> ""
            CondEQ -> "eq"
            CondNE -> "ne"
            CondLT -> "lt"
            CondGE -> "ge"

asmMop :: MemOp -> Text
asmMop mop = case mop of
    DerefReg r      -> "[" <> asmReg r <> "]"
    MemAddress n    -> "=" <> T.pack (show n)
    MemLabel l      -> "=" <> asmLabel l
    MemOffset (r,o) -> "[" <> asmReg r <> "," <> asmFlexOff o <> "]"

asmFlexOff :: FlexOffset -> Text
asmFlexOff n = "#" <> T.pack (show n) -- temp

asmDirective :: Directive -> Text
asmDirective d = case d of
    Global l -> "global " <> asmLabel l

asmLabel :: Label -> Text
asmLabel = coerce

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
    R12 -> "ip"
    R13 -> "r13"
    R14 -> "lr"
    R15 -> "pc"

asmRegSet :: RegSet -> Text
asmRegSet (RegSingle r) = asmReg r
asmRegSet (RegSet rs) = "{" <> mconcat (intersperse ", " regs) <> "}"
    where regs = fmap asmReg rs

asmOp2 :: Operand2 -> Text
asmOp2 (FlexImmediate n) = "#" <> asmImm n
asmOp2 (FlexRegister r Nothing) = asmReg r
asmOp2 (FlexRegister r (Just s)) = asmReg r <> ", " <> asmShift s

asmImm :: Immediate -> Text
asmImm (ImmInt n) = T.pack $ show n
asmImm (ImmChar c) = T.pack $ show c

asmShift :: Shift -> Text
asmShift s = case s of
    ASR n   -> "ASR " <> imm n
    LSL n   -> "LSL " <> imm n
    LSR n   -> "LSR " <> imm n
    ROR n   -> "ROR " <> imm n
    RRX     -> "RRX"

    where imm = asmOp2 . toOperand2

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

sub :: (FlexibleOperand op2) => Reg -> Reg -> op2 -> ARM ()
sub rd rs op2 = emiti $ Sub rd rs (toOperand2 op2)

mul :: Reg -> Reg -> Reg -> ARM ()
mul rd rm rs = emiti $ Mul rd rm rs

udiv :: Reg -> Reg -> Reg -> ARM ()
udiv rd rm rs = emiti $ Udiv rd rm rs

mov :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
mov rd op2 = emiti $ Mov Uncond rd (toOperand2 op2)

moveq :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
moveq rd op2 = emiti $ Mov CondEQ rd (toOperand2 op2)

movne :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
movne rd op2 = emiti $ Mov CondNE rd (toOperand2 op2)

movlt :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
movlt rd op2 = emiti $ Mov CondLT rd (toOperand2 op2)

movge :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
movge rd op2 = emiti $ Mov CondGE rd (toOperand2 op2)

push :: (FlexibleRegSet a) => a -> ARM ()
push rs = emiti $ Push (toRegSet rs)

pop :: (FlexibleRegSet a) => a -> ARM ()
pop rs = emiti $ Pop (toRegSet rs)

cmp :: (FlexibleOperand op2) => Reg -> op2 -> ARM ()
cmp rn op2 = emiti $ Cmp rn (toOperand2 op2)

-- i use `b` as an identifier WAY too often for this to conflict... sorry
branch :: Label -> ARM ()
branch l = emiti $ Branch Uncond l

beq :: Label -> ARM ()
beq l = emiti $ Branch CondEQ l

bne :: Label -> ARM ()
bne l = emiti $ Branch CondNE l

bl :: Label -> ARM ()
bl l = emiti $ Bl l

ldr :: (FlexibleMemOp mop) => Reg -> mop -> ARM ()
ldr rd ms = emiti $ Ldr rd (toMemOp ms)

str :: (FlexibleMemOp mop) => Reg -> mop -> ARM ()
str rs ms = emiti $ Str Uncond rs (toMemOp ms)
--------------------------------------------------------------------------------

-- | push and preserve dword alignment
pusha :: Reg -> ARM ()
pusha rs = emiti $ Push (RegSet [rs, ip])

infixl 9 #

-- | used to avoid ambiguity errors with Num and FlexibleOperand
-- in a way that is remenicent of gas's arm syntax
(#) :: (Int -> b) -> Int -> b
f # x = f x

