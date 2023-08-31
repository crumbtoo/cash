module CodeGen
    ( emit
    )
    where
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Foldable                 (traverse_)
import ARM
import AST
--------------------------------------------------------------------------------
class CodeGen a where
    -- | emits some assembly that returns to r0
    emit :: a -> ARM ()
--------------------------------------------------------------------------------

instance CodeGen Stat where
    emit (FunctionStat vis name pars body) = do
        let l = toLabel name

        when (vis == Public) $
            global l
        label l
        
        push [fp, lr]
        emit body
        -- mov r0 #0
        pop [fp, pc]

    emit (BlockStat ss) = traverse_ emit ss

    emit (ReturnStat e) = do
        -- evaluates into r0
        emit e
        pop [fp, pc]

instance CodeGen [Stat] where
    emit = traverse_ emit

instance CodeGen Expr where
    emit (LitNum n) = mov r0 n

