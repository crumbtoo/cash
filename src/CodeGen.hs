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
    -- | emits some assembly that returns to r0
    emit :: a -> ARM ()
--------------------------------------------------------------------------------

instance CodeGen Stat where
    emit (FunctionStat vis name pars body) = do
        comment $ printf "function %s()" name
        let l = toLabel name
        
        when (vis == Public) $
            global l
        label l
        
        push [fp, lr]
        emit body
        -- mov r0 #0
        pop [fp, pc]

    emit (BlockStat ss) = traverse_ emit ss

    emit (AssertStat e) = do
        comment "assert"
        emit e
        cmp r0 #1
        moveq r0 '.'
        movne r0 'F'
        bl "putchar"
        
    emit (ReturnStat e) = do
        comment "return"
        -- evaluates into r0
        emit e
        pop [fp, pc]

instance CodeGen [Stat] where
    emit = traverse_ emit

instance CodeGen Expr where
    emit (LitNum n) = mov r0 n

    emit (Equal a b) = do
        emit a
        mov r0 r1
        emit b
        cmp r0 r1
        moveq r0 #1
        movne r0 #0

