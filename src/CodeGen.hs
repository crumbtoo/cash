{-# LANGUAGE OverloadedStrings #-}
module CodeGen
    ( emit
    )
    where
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Foldable                  (traverse_)
import           Text.Printf                    (printf)
import ARM
import AST
--------------------------------------------------------------------------------
class CodeGen a where
    -- | emit into a given register; default will not be effecient
    emitTo :: Reg -> a -> ARM ()
    -- | emits some assembly
    emit :: a -> ARM ()

    emitTo rd a = emit a >> mov r0 rd
    emit = emitTo r0
    {-# MINIMAL emit | emitTo #-}
--------------------------------------------------------------------------------

instance CodeGen Stat where
    emitTo _ (FunctionStat vis name pars body) = do
        comment $ printf "function %s()" name
        let l = toLabel name
        
        when (vis == Public) $
            global l
        label l
        
        push [fp, lr]
        emit body
        pop [fp, pc]

    emitTo _ (BlockStat ss) = traverse_ emit ss

    emitTo rd (AssertStat e) = do
        comment "assert"
        emitTo r0 e
        cmp r0 #1
        moveq r0 '.'
        movne r0 'F'
        bl "putchar"
        
    emitTo rd (ReturnStat e) = do
        comment "return"
        -- functions should return to r0
        emitTo r0 e

instance CodeGen Expr where
    emitTo rd (LitNum n) = comment "litnum" >> ldr rd n

    emitTo rd (Equal a b) = do
        comment "=="
        binop a b $ do
            cmp r0 r1
            moveq rd #1
            movne rd #0

    emitTo rd (Multiply a b) = do
        comment "*"
        binop a b $ do
            mul rd r0 r1

    emitTo rd (Not e) = do
        comment "!"
        emitTo rd e
        cmp rd #1
        moveq rd #0
        movne rd #1

-- operands placed in r0 and r1
binop :: (CodeGen a, CodeGen b) => a -> b -> ARM () -> ARM ()
binop a b asm = do
    emitTo r0 a
    push [r0, ip]
    emitTo r1 b
    pop [r0, ip]

    asm

